#' @keywords datagen
#' @export
#' @title Convert NBI 2016 ASCII to Tibble
#' @param filePath location of the NBI 2016 ASCII CSV file
#' @description A previously downloaded NBI ASCII CSV file for the 2016 version of 
#' \href{https://www.fhwa.dot.gov/bridge/nbi/2016hwybronlyonefile.zip}{Highway Bridges for all States}
#' is converted to a tibble with careful QC and renaming of the columns of data.
#' 
#' Columns in the returned tibble include:
#' \enumerate{
#' \item{stateCode} -- ISO 3166-2 alpha-2
#' \item{longitude} -- decimal degrees E
#' \item{latitude} -- decimal degrees N
#' }
#' @details The NBI 2016 dataset can be downloaded from the Federal Highway Administration
#' with the following command:
#' \preformatted{
#' curl https://www.fhwa.dot.gov/bridge/nbi/2016hwybronlyonefile.zip -O
#' }
#' This file should be left in '.zip' format.
#' 
#' @return Tibble containing QC'ed NBI data.
#' @references \href{https://www.fhwa.dot.gov/bridge/nbi/ascii.cfm}{NBI ASCII data}
#' @references \href{https://www.fhwa.dot.gov/bridge/mtguide.pdf}{NBI Recording and Coding Guide}

convert2016 <- function(filePath=NULL) {
  
  # Sanity check
  if ( is.null(filePath) ) stop("Required parameter 'filePath' is missing.")

  # ----- Ingest --------------------------------------------------------------
  
  # Read everything in as character data to avoid any 'problems(...)'
  col_types <- paste0(rep('c',135),collapse='')
  rawDF <- readr::read_csv(filePath, col_types=col_types)
  
  # Create a minimal nbi 'tibble' that has the proper number of rows
  nbi <- rawDF[,'STATE_CODE_001']
  
  # ----- stateCode -------------------------------------------------------------
  
  # From the coding guide:
  #
  #   Item 1 - State Code     3 digits
  #   The first 2 digits are the Federal Information Processing Standards
  #   (FIPS) code for States, and the third digit is the FHWA region code.
  #   (New Jersey and New York will retain an FHWA region code of 2.)
  
  # NOTE:  despite the coding guide, the state codes are only two characters long
  # NOTE:  sort(unique(stringr::str_count(rawDF$STATE_CODE_001)))
  
  fips <- rawDF$STATE_CODE_001
  
  # Create a vector of state codes, named by fips codes
  stateByFips <- as.character(maps::state.fips$abb)
  names(stateByFips) <- sprintf("%02d", maps::state.fips$fips)
  stateByFips <- c(stateByFips, '02'='AK')
  stateByFips <- c(stateByFips, '15'='HI')
  stateByFips <- c(stateByFips, '72'='PR')
  
  nbi$stateCode <- stateByFips[fips]
  
  # Now remove the unneeded 'STATE_CODE_001'
  nbi$STATE_CODE_001 <- NULL
  
  # > barplot(sort(table(nbi$stateCode)), horiz=TRUE, las=1)
  
  # ----- countyCode ------------------------------------------------------------
  
  # TODO: add countyCode

  # ----- latitude and longitude ------------------------------------------------
  
  # TODO:  Pull out latDMS and lonDMS at the beginning and include information
  # TODO:  from the coding guide.
  
  # NOTE:  We pull out latDMS and lonDMS at this point because there are many 
  # NOTE:  cases where we have to swap the data from those columns
  
  # ----- latitude --------------------------------------------------------------
  
  # From the coding guide:
  #
  #   Item 16 - Latitude  (XX degrees XX minutes XX.XX seconds)     8 digits
  #   For bridges on STRAHNET and STRAHNET Connector highways and on the NHS,
  #   record and code the latitude of each in degrees, minutes and seconds to
  #   the nearest hundredth of a second (with an assumed decimal point).
  
  latDMS <- rawDF$LAT_016
  
  # Convert NA to '00000000'
  latDMS[is.na(latDMS)] <- '00000000'
  
  # ----- Check for string length -----
  
  # > table(stringr::str_count(latDMS))
  #
  # 7      8 
  # 1 614386 
  #
  # > latDMS[stringr::str_count(latDMS) == 7]
  # [1] "3604002"
  # > nbi[stringr::str_count(latDMS) == 7,]
  # A tibble: 1 x 1
  #   stateCode
  #       <chr>
  # 1        NV
  #
  # This Nevada lattude is missing it's last character -- just add '0'
  latDMS[333453] <- paste0(latDMS[333453], '0')

  # ----- Check for non-numerics -----
  
  # > latDMS[stringr::str_detect(latDMS, "[^0-9]")]
  # [1] "643659.8" "390406.4"
  # > nbi[stringr::str_detect(latDMS, "[^0-9]"),]
  # A tibble: 2 x 1
  # stateCode
  #     <chr>
  # 1      AK
  # 2      OH
  
  # These are in Alaska and Ohio, respectively. It looks like someone used regular decimal notation
  # so we need to replace ".8" with "80" to match the official encoding. Or we can just remove the
  # decimal point before converting to numeric which has the same effect.
  latDMS <- stringr::str_replace(latDMS, "[.]", "")
  
  # Convert '00000000' to NA
  badMask <- stringr::str_detect(latDMS,'00000000')
  latDMS[badMask] <- NA
  
  # ----- Check the domain -----

  # > hist(as.numeric(latDMS), n=100)
  # > rug(as.numeric(latDMS))
  
  # ----- Deal with high latitudes
  highLatMask <- !is.na(latDMS) & as.numeric(latDMS) > 7.5e7

  # > which(highLatMask)
  # [1]  15576  15873  29670 235524 238471 239296 239303 239683 240069 354266 467616
  # > cbind(nbi[highLatMask,],rawDF[highLatMask, c('STATE_CODE_001','LAT_016','LONG_017')])
  # stateCode STATE_CODE_001  LAT_016  LONG_017
  # 1         AL             01 87304350 033122220
  # 2         AL             01 88023877 030400018
  # 3         AR             05 91520700  33510500
  # 4         ME             23 99999999 999999999
  # 5         MD             24 99999999 076411000
  # 6         MD             24 77150000 039250000
  # 7         MD             24 99999999 077212000
  # 8         MD             24 77503000 038820000
  # 9         MD             24 99999999 076549000
  # 10        NY             36 76175370 043161380
  # 11        PA             42 80180600 040360000
  
  # None of these are in Alaska and several different types of mistakes are found: 
  # 4, 5, 7, and 9 are using '999999999' as bad flag instead of 000000000
  # The rest have swapped lat/lon
  
  # 15576 -- swap lat/lon
  latDMS[15576] <- stringr::str_sub(rawDF$LONG_017[15576],2,9)
  lonDMS[15576] <- paste0('0',rawDF$LAT_016[15576])
  # 15873 -- swap lat/lon
  latDMS[15873] <- stringr::str_sub(rawDF$LONG_017[15873],2,9)
  lonDMS[15873] <- paste0('0',rawDF$LAT_016[15576])
  # 29670 -- swap lat/lon
  latDMS[29670] <- rawDF$LONG_017[29670]
  lonDMS[29670] <- paste0('0',rawDF$LAT_016[29670])
  # 235524 -- change 999999999 to NA
  latDMS[235524] <- NA
  lonDMS[235524] <- NA
  # 238471 -- change 999999999 to NA
  latDMS[238471] <- NA
  # 239296 -- swap lat/lon
  latDMS[239296] <- stringr::str_sub(rawDF$LONG_017[239296],2,9)
  lonDMS[239296] <- paste0('0',rawDF$LAT_016[239296])
  # 239303 -- change 99999999 to NA
  latDMS[239303] <- NA
  # 239683 -- swap lat/lon
  latDMS[239683] <- stringr::str_sub(rawDF$LONG_017[239683],2,9)
  lonDMS[239683] <- paste0('0',rawDF$LAT_016[239683])
  # 240069 -- change 99999999 to NA
  latDMS[240069] <- NA
  # 354266 -- swap lat/lon
  latDMS[354266] <- stringr::str_sub(rawDF$LONG_017[354266],2,9)
  lonDMS[354266] <- paste0('0',rawDF$LAT_016[354266])
  # 467616 -- swap lat/lon
  latDMS[467616] <- stringr::str_sub(rawDF$LONG_017[467616],2,9)
  lonDMS[467616] <- paste0('0',rawDF$LAT_016[467616])

  # ----- Deal with low latitudes
  lowLatMask <- !is.na(latDMS) & as.numeric(latDMS) < 1e7
    
  # > hist(as.numeric(latDMS[lowLatMask]), n=100, xlim=c(0,1e7))
  # > rug(as.numeric(latDMS[lowLatMask]))

  # > cbind(nbi[lowLatMask,],rawDF[lowLatMask, c('STATE_CODE_001','LAT_016','LONG_017')])
  # stateCode STATE_CODE_001  LAT_016  LONG_017
  # 1           AL             01 03117246 088233077
  # 2           AL             01 03342040 085492220
  # 3           AL             01 03135310 087584290
  # 4           AK             02 643659.8 1621516.7
  # 5           AZ             04 03451000 111376000
  # 6           AZ             04 03407710 112574539
  # 7           AZ             04 03201000 111365000
  # 8           AZ             04 03432000 111585000
  # 9           AZ             04 03251217 010947767
  # 10          AZ             04 03346440 112053952
  # 11          CA             06 03733192 121152820
  # 12          CO             08 03835011 105061409
  # 13          CO             08 00000100 000000100
  # 14          CO             08 00000100 000000100
  # 15          CO             08 03905460 108300050
  # 16          DC             11 00385449 077032271
  # 17          IA             19 00411646 000954822
  # 18          IA             19 00421347 000944513
  # 19          IA             19 00423214 000945008
  # ...

  # Again, a variety of bugs but one of the more common problems, especially with Maryland
  # is that they added "00" to the *beginning* of the lat/lon strings rather than at the end.
  
  mLowLatMask <- !is.na(latDMS) & as.numeric(latDMS) <1e7 & as.numeric(latDMS) > 1e6
  
  # cbind(nbi[mlowLatMask,],latitudes=latDMS[mlowLatMask],longitudes=lonDMS[mlowLatMask],nrow=which(mlowLatMask))
  # stateCode latitudes longitudes   nrow
  # 1         AL  03117246  088233077   4846
  # 2         AL  03342040  085492220  14764
  # 3         AL  03135310  087584290  15470
  # 4         AK   6436598  1621516.7  17127
  # 5         AZ  03451000  111376000  18979
  # 6         AZ  03407710  112574539  20467
  # 7         AZ  03201000  111365000  20689
  # 8         AZ  03432000  111585000  22156
  # 9         AZ  03251217  010947767  23247
  # 10        AZ  03346440  112053952  24964
  # ...
  # For lats between 1e6 and 1e7, it appears that the leading 0 should be at the end.
  
  latDMS[mLowLatMask] <- ifelse(stringr::str_length(latDMS[mLowLatMask]) == 8, 
                                paste0(stringr::str_sub(latDMS[mLowLatMask],2,8),0),
                                paste0(latDMS[mLowLatMask],0))
  
  # > newlowLatMask <- !is.na(latDMS) & as.numeric(latDMS) < 1e7
  # Warning messages:
  #   1: Unknown or uninitialised column: 'latitude'. 
  # 2: Unknown or uninitialised column: 'longitude'. 
  # 3: Unknown or uninitialised column: 'latitude'. 
  # 4: Unknown or uninitialised column: 'longitude'. 
  # > table(nbi$stateCode[newlowLatMask])
  # 
  # CO   DC   IA   KY   MD   MT   ND   NJ 
  # 2    1   22    1 1920    1    1    1 
  
  
  
  
  # > table(nbi$stateCode[lowLatMask])
  # 
  # AK   AL   AZ   CA   CO   DC   IA   KY   MD   MT   ND   NJ   OH   PA   PR 
  # 1    3    6    1    4    1   22    1 1936    2    2    3    1    1    2 
 
  # TODO:  Fix consistent latitude/longitude issues with Maryland
  
  # TODO:  Fix last few domain-related latitude issues
  
  
  
  # ----- Convert to decmial degrees -----
  deg <- as.numeric(stringr::str_sub(latDMS, 1, 2))
  min <- as.numeric(stringr::str_sub(latDMS, 3, 4))
  sec <- as.numeric(stringr::str_sub(latDMS, 5, 8))/100
  
  nbi$latitude <- deg + min/60 + sec/3600
  
  
  # Remove values that are out of domain
  
  badMask <- nbi$latitude < 10 | nbi$latitude > 80
  nbi$latitude[badMask] <- NA
  
  # ----- longitude -------------------------------------------------------------
  
  #   Item 17 - Longitude  (XX degrees XX minutes XX.XX seconds)     9 digits
  #   For bridges on STRAHNET and STRAHNET Connector highways and on the NHS,
  #   record and code the longitude of each in degrees, minutes and seconds to
  #   the nearest hundredth of a second (with an assumed decimal point).  A
  #   leading zero shall be coded where needed.
  
  lonDMS <- rawDF$LONG_017
  
  # > sum(is.na(lonDMS))
  # [1] 8
  
  # Convert NA to '000000000'
  lonDMS[is.na(lonDMS)] <- '000000000'
  
  # TODO:  Need to be much more careful about how we correct longitudes
  
  # Find and clean longitudes which contain a "."
  
  # lonDMS[dotMask]
  # > lonDMS[dotMask]
  # [1] "1621516.7" "840222.66"
  # > sum(str_detect(lonDMS, "[.]"))
  # [1] 2 
  # 
  # They are in the same rows as for longitudes, and it once again appears that they should simply be deleted.
  
  lonDMS <- stringr::str_replace(lonDMS, "[.]", "")
  
  # > table(stringr::str_count(lonDMS))
  # 
  # 8      9 
  # 16879 597500 
  #
  # > which(stringr::str_count(lonDMS) == 8)[1:3]
  # [1] 25741 25742 25743
  #
  # Looks like many 8-char values are missing the initial '0'
  #
  # hist(as.numeric(lonDMSshortMask])) -- yep, mostly 80 and 90 degrees with a couple of zeros we'll remove later
  
  # > lonDMS[shortMask][as.numeric(lonDMS[shortMask])< 60000000]
  # [1] "033510500" "000000000" "007251031" 
  
  # "07251031" was missing its last character.
  
  lonDMS[563454] <- paste0(lonDMS[563454], 0)
  
  # > rawDF[29670, c("LAT_016", "LONG_017")]
  # # A tibble: 1 x 2
  # LAT_016 LONG_017
  # <chr>    <chr>
  #   1 91520700 33510500  This is in Arkansas so latitude and longitude are flipped, and missing an initial zero.
  
  lonDMS[29670] <- rawDF[29670, "LAT_016"]
  
  shortMask <- stringr::str_count(lonDMS) == 8
  lonDMS[shortMask] <- paste0('0', lonDMS[shortMask])
  
  
  # NOTE:  these are degrees W
  
  deg <- as.numeric(stringr::str_sub(lonDMS, 1, 3))
  min <- as.numeric(stringr::str_sub(lonDMS, 4, 5))
  sec <- as.numeric(stringr::str_sub(lonDMS, 6, 9))/100
  
  nbi$longitude <- -1 * (deg + min/60 + sec/3600)
  
  # Remove values that are out of domain
  
  badMask <- nbi$longitude < -180 | nbi$longitude > -10
  nbi$longitude[badMask] <- NA
  
  # -----------------------------------------------------------------------------
  # START QUICK TEST
  if ( FALSE ) {
    
    wa <- dplyr::filter(nbi, stateCode == 'WA')
    dim(wa)
    plot(wa$longitude, wa$latitude, pch=0)
    which(wa$longitude > -100)
    wa <- wa[-3531,]
    plot(wa$longitude, wa$latitude, pch=0)
    
  }
  # END QUICK TEST
  # -----------------------------------------------------------------------------
  
  #---- yearBuilt ----------------------------------------------------------------
  nbi$yearBuilt <- as.numeric(rawDF$YEAR_BUILT_027) #adding the year the bridge was built
  nbi$age <- 2017 - nbi$yearBuilt
  #------------------------------------------------------------------------------
  #---- averageCarCount ---------------------------------------------------------
  nbi$averageCarCount <- as.numeric(rawDF$ADT_029)
  #-----------------------------------------------------------------------------
  #---- water (indicates whether the bridge goes over water)---------------------
  
  # N Not applicable. Use when bridge is not over a waterway (channel).
  # 9 There are no noticeable or noteworthy deficiencies which affect the condition of the channel.
  # 8 Banks are protected or well vegetated. River control devices such as spur dikes and embankment
  #   protection are not required or are in a stable condition.
  # 7 Bank protection is in need of minor repairs. River control devices and embankment protection 
  #   have a little minor damage. Banks and/or channel have minor amounts of drift.
  # 6 Bank is beginning to slump. River control devices and embankment protection have widespread 
  #   minor damage. There is minor stream bed movement evident. Debris is restricting the channel slightly.
  # 5 Bank protection is being eroded. River control devices and/or embankment have major damage. 
  #   Trees and brush restrict the channel.
  # 4 Bank and embankment protection is severely undermined. River control devices have severe damage. 
  #   Large deposits of debris are in the channel.
  # 3 Bank protection has failed. River control devices have been destroyed. Stream bed aggradation,
  #   degradation or lateral movement has changed the channel to now threaten the bridge and/or approach roadway.
  # 2 The channel has changed to the extent the bridge is near a state of collapse.
  # 1 Bridge closed because of channel failure. Corrective action may put back in light service.
  # 0 Bridge closed because of channel failure. Replacement necessary. 
  
  nbi$Waterway <- rawDF$WATERWAY_EVAL_071
  nbi$water <- ifelse(nbi$Waterway == "N", 0, 1) 
  nbi$waterwayAdequacy <- as.numeric(nbi$Waterway)
  nbi$Waterway <- NULL
  #------------------------------------------------------------------------------
  # #---------laneCount------------------------------------------------------------
  # # There are so many data points that are apparently totally wrong that this variable may not be usable.
  # # Roadway width will give us similar information.
  # nbi$laneCount <- as.numeric(rawDF$TRAFFIC_LANES_ON_028A)
  # nbi$underLaneCount <- as.numeric(rawDF$TRAFFIC_LANES_UND_028B)
  # #----------------------
  #-------------channelCondition----------------------------------------------
  nbi$channelCondition <- as.numeric(rawDF$CHANNEL_COND_061)
  
  return(nbi)
  
}
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
  
  # barplot(sort(table(nbi$stateCode)), horiz=TRUE, las=1)
  
  # ----- countyCode ------------------------------------------------------------
  
  # TODO
  
  # ----- latitude --------------------------------------------------------------
  
  # From the coding guide:
  #
  #   Item 16 - Latitude  (XX degrees XX minutes XX.XX seconds)     8 digits
  #   For bridges on STRAHNET and STRAHNET Connector highways and on the NHS,
  #   record and code the latitude of each in degrees, minutes and seconds to
  #   the nearest hundredth of a second (with an assumed decimal point).
  
  latDMS <- rawDF$LAT_016
  
  # Convert NA to '000000000'
  latDMS[is.na(latDMS)] <- '000000000'
  
  # > table(stringr::str_count(latDMS))
  #
  # 7      8 
  # 1 614379 
  #
  # > which(stringr::str_count(latDMS) == 7)
  # [1] 333453
  #
  # row 333453 has a latitude that is missing it's last character -- just add '0'
  latDMS[333453] <- paste0(latDMS[333453], '0')
  
  # TODO:  More careful assessment of latitude values
  
  # latNum <- as.numeric(latDMS)
  # hist(latNum[latNum<1e7], n=10)
  #
  # looking at (latNum > 0) & (latNum < 1e6) we see record 17127 which has:
  #
  # str(rawDF[17127,])
  # ...
  # $ LAT_016                : chr "643659.8"   # state is Alaska so this is 64.3 N, -162.1 E
  # $ LONG_017               : chr "1621516.7"
  # ...
  #
  # So we need to trap and clean up any latitudes that have a '.'
  
  dotMask <- str_detect(latDMS, "[.]")
  # > latDMS[dotMask]
  # [1] "643659.8" "390406.4"
  # 
  # These are in Alaska and Ohio, respectively. So the decimal point is likely just a typo and should be removed.
  latDMS <- str_replace(latDMS, "[.]", "")
  
  
  # > rawDF[29670, c("LAT_016", "LONG_017")]
  # # A tibble: 1 x 2
  # LAT_016 LONG_017
  # <chr>    <chr>
  #   1 91520700 33510500  This is in Arkansas so latitude and longitude are flipped, and missing an initial zero.
  
  latDMS[29670] <- rawDF[29670, "LONG_017"]
  
  # Convert '00000000' to NA
  badMask <- stringr::str_detect(latDMS,'00000000')
  latDMS[badMask] <- NA
  
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
  
  lonDMS <- str_replace(lonDMS, "[.]", "")
  
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
  #------------------------------------------------------------------------------
  #---- averageCarCount ---------------------------------------------------------
  nbi$averageCarCount <- as.numeric(rawDF$ADT_029)
  #-----------------------------------------------------------------------------
  return(nbi)
  
}
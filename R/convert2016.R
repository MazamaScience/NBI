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
#' @examples
#' \dontrun{
#' nbi <- convert2016('~/Data/NBI/2016hwybronlyonefile.zip')
#' }
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
  lonDMS <- rawDF$LONG_017
  
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
  # This Nevada lattude is missing its last character -- just add '0'
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
  
  # 15576, 15873, 29670, 239296, 239683, 354266, 354266, 467616-- swap lat/lon
  latDMS[c(15576, 15873, 29670, 239296, 239683, 354266, 354266, 467616)] <- 
    stringr::str_sub(rawDF$LONG_017[c(15576, 15873, 29670, 239296, 239683, 354266, 354266, 467616)],2,9)
  lonDMS[c(15576, 15873, 29670, 239296, 239683, 354266, 354266, 467616)] <- 
    paste0('0',rawDF$LAT_016[c(15576, 15873, 29670, 239296, 239683, 354266, 354266, 467616)])
  # 235524, 238471, 239303, 240069  -- change 999999999 to NA
  latDMS[c(235524, 238471, 239303, 240069)] <- NA
  lonDMS[c(235524, 238471, 239303, 240069)] <- NA

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
  
  # > newLowLatMask <- !is.na(latDMS) & as.numeric(latDMS) < 1e7
  # > table(nbi$stateCode[newLowLatMask])
  # 
  # CO   DC   IA   KY   MD   MT   ND   NJ 
  # 2    1   22    1 1920    1    1    1 
  # > lowlatdf <- data.frame(nbi[newLowLatMask,], latitude = latDMS[newLowLatMask], 
  # longitude = lonDMS[newLowLatMask], row = which(newLowLatMask), stringsAsFactors = FALSE)
  #                          + )
  # > subset(lowlatdf, stateCode != "MD" & stateCode != "IA")
  #      stateCode latitude longitude    row
  # 1           CO 00000100 000000100  68661
  # 2           CO 00000100 000000100  70138
  # 3           DC 00385449 077032271  77838
  # 26          KY 00000100 000000100 212456
  # 1947        MT 00481055 001142443 312113
  # 1948        NJ 00007470 074415853 343401
  # 1949        ND 00000100 000000100 383522
  
  # Someone has used 00000100 as the bad flag. It looks like the '0's for DC and MT should be at the 
  # end. The faulty NJ latitude may be unsalvageable. MT longitude MAYBE is supposed to be '104244300'.
  
  # 68661, 70138, 212456, 383522 change lat and lon to NA
  latDMS[c(68661, 70138, 212456, 383522)] <- NA
  lonDMS[c(68661, 70138, 212456, 383522)] <- NA
  # 77838, 312113 move leading 0s to end
  latDMS[c(77838, 312113)] <- paste0(stringr::str_sub(latDMS[c(77838, 312113)],3,8),"00")
  
  
  # Move two leading 0s to end
  newLowLatMask <- !is.na(latDMS) & as.numeric(latDMS) < 1e6 & as.numeric(latDMS) >1e5
  latDMS[newLowLatMask] <- paste0(stringr::str_sub(latDMS[newLowLatMask],3,8), "00")
  
  # Fix remaining issues
  newLowLatMask <- !is.na(latDMS) & as.numeric(latDMS) < 1e7
    # cbind(nbi[newLowLatMask,],latitudes=latDMS[newLowLatMask],longitudes=lonDMS[newLowLatMask],nrow=which(newLowLatMask))
  # stateCode latitudes longitudes   nrow
  # 1         MD  00039007  000076297 235749
  # 2         MD  00039007  000076297 235750
  # 3         MD  00038579  000076380 235771
  # 4         MD  00039007  000076297 235781
  # 5         MD  00039095  000076418 235791
  # 6         MD  00039121  000076369 235862
  # 7         MD  00039420  000078160 235895
  # 8         MD  00039224  000076293 236027
  # 9         MD  00039208  000076297 236172
  # 10        MD  00039151  000076274 236286
  # 11        MD  00039151  000076274 236287
  # 12        MD  00039144  000076284 236353
  # 13        MD  00000038  -00000007 236616
  # 14        MD  00039310  000076095 236964
  # 15        MD  00000003  000780800 237055
  # 16        MD  00039032  000076033 237716
  # 17        MD  00075413  000038130 237803
  # 18        MD  00039144  000076704 238214
  # 19        MD  00039367  000076410 238305
  # 20        MD  00000039  000000077 238727
  # 21        MD  00000040  -00000007 239104
  # 22        MD  00038550  000076460 240150
  # 23        MD  00000039  000000077 240713
  # 24        NJ  00007470  074415853 343401
  
  # most can be fixed by moving 3 leading 0s to end. 237803 also has lat/lon confused.
  latDMS[237803] <- stringr::str_sub(rawDF$LONG_017[237803],2,9)
  lonDMS[237803] <- paste0("0",rawDF$LAT_016[237803])
  latDMS[newLowLatMask] <- paste0(stringr::str_sub(latDMS[newLowLatMask],4,8),"000")
  
  
  
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

  
  # > sum(is.na(lonDMS))
  # [1] 8
  
  # Convert NA to '000000000'
  lonDMS[is.na(lonDMS)] <- '000000000'
  
  # TODO:  Need to be much more careful about how we correct longitudes
  
  # Find and clean longitudes which contain a non-number
  
  # badLons <- stringr::str_detect(lonDMS, "[^0-9]")
  # cbind(nbi[badLons,], lonDMS[badLons], latDMS[badLons], which(badLons))
  # stateCode lonDMS[badLons] latDMS[badLons] which(badLons)
  # 1        AK       1621516.7        64365980          17127
  # 2        CT       -73302410        41354213          76928
  # 3        MD       -00000007        00038000         236616
  # 4        MD       -07639055        39062984         238225
  # 5        MD       -07663107        39269531         238781
  # 6        MD       -00000007        00040000         239104
  # 7        MD       -07579773        38353769         240403
  # 8        OH       840222.66        39040640         391680
  
  # There are a couple where decimal notation was used, and we just need to
  # remove the "." and a couple where it is (negative) degrees east instead of
  # (positive) degrees west, and a couple which look unsalvageable.
  
  # 17127, 391680 convert from decimal notation
  lonDMS[17127] <- paste0(stringr::str_replace(lonDMS[17127],"[.]",""),"0")
  lonDMS[391680] <- paste0("0", stringr::str_replace(lonDMS[391680], "[.]",""))
  
  # 76928, 238225, 238781, 240403 remove "-" and add leading or trailing "0"
  lonDMS[76928] <- paste0("0", stringr::str_sub(lonDMS[76928], 2,9))
  lonDMS[c(238225, 238781, 240403)] <- paste0(stringr::str_sub(lonDMS[c(238225, 238781, 240403)], 2,9), "0")
  
  # 236616, 239104 are unsalvageable. 
  lonDMS[c(236616, 239104)] <- "000000000"
  
  # > table(stringr::str_count(lonDMS))
  # 
  # 8      9 
  # 16878 597509 
  #
  # > which(stringr::str_count(lonDMS) == 8)[1:3]
  # [1] 25741 25742 25743
  #
  # Looks like many 8-char values are missing the initial '0'
  #
  # hist(as.numeric(lonDMSshortMask])) -- yep, mostly 80 and 90 degrees with a couple of zeros we'll remove later
  
  # > lonDMS[shortMask][as.numeric(lonDMS[shortMask])< 60000000]
  # [1] "000000000" "07251031" 
  
  # "07251031" was missing its last character.
  
  lonDMS[563454] <- paste0(lonDMS[563454], 0)
  
  shortMask <- stringr::str_count(lonDMS) == 8
  lonDMS[shortMask] <- paste0('0', lonDMS[shortMask])
  
  # check for out-of-domain longitudes
  highLonMask <- as.numeric(lonDMS) > 1.8e8
  # > lonDMS[highLonMask]
  # [1] "863832130" "765833300" "770100000" "770301400" "934032380"
  # [6] "933958820" "933950760" "933517040" "933910020" "933903230"
  # [11] "933933510" "933707000" "933707670" "933916700" "933612730"
  # [16] "760534586" "763840000" "760261940" "771700000" "772433000"
  # [21] "761041400" "754727720" "764311730" "763755998" "705274592"
  # [26] "870402900" "933224800" "933217260" "933211100" "654444390"
  # [31] "655560104" "660941300" "655590190" "660730000" "654525520"
  # 
  # They all appear to be missing a leading "0". 
  lonDMS[highLonMask] <- paste0("0", stringr::str_sub(signif(as.numeric(lonDMS)[highLonMask], 8), 1, 8))
  
  badMask <- stringr::str_detect(lonDMS,'00000000')
  lonDMS[badMask] <- NA
  lowLonMask <- !is.na(lonDMS) & as.numeric(lonDMS) < 1e7
  # table(nbi$stateCode[lowLonMask])
  # 
  # AL   AR   AZ   CO   FL   IA   MD   MT   NC   ND   NH   NJ   PA   PR   SD   TX   VA   VT 
  # 1    1    4    1    1   22 1932    1    1    3    1    1    3    6    2    1    1    1 
  
  # > lowLonNotiamd <- !is.na(lonDMS) & as.numeric(lonDMS) < 1e7 & nbi$stateCode != "MD" & nbi$stateCode != "IA"
  # > cbind(stateCode = nbi$stateCode[lowLonNotiamd], longitude = lonDMS[lowLonNotiamd], 
  #         latitude = latDMS[lowLonNotiamd], row = which(lowLonNotiamd))
  #       stateCode longitude   latitude   row     
  # [1,] "AL"      "008608970" "31063530" "15461" 
  # [2,] "AZ"      "009560903" "34105303" "20112" 
  # [3,] "AZ"      "001100010" "31574330" "25286" 
  # [4,] "AZ"      "001100040" "31574459" "25287" 
  # [5,] "AZ"      "001100070" "31574590" "25288" 
  # [6,] "AR"      "004090000" "34251204" "38492" 
  # [7,] "CO"      "000003883" "38494744" "69486" 
  # [8,] "FL"      "008523630" "30184030" "90372" 
  # [9,] "MT"      "001142443" "48105500" "312113"
  # [10,] "NH"      "001110938" "44025294" "337120"
  # [11,] "NJ"      "007413736" "40449710" "342990"
  # [12,] "NC"      "004123607" "35143887" "383158"
  # [13,] "ND"      "000973129" "46510204" "383913"
  # [14,] "ND"      "000972204" "48490497" "385391"
  # [15,] "ND"      "009740265" "46365789" "385490"
  # [16,] "PA"      "007504090" "40255320" "451134"
  # [17,] "PA"      "007943120" "40201200" "466309"
  # [18,] "PA"      "007514175" "40015290" "469580"
  # [19,] "SD"      "003421197" "44041556" "485980"
  # [20,] "SD"      "003431496" "44040888" "485981"
  # [21,] "TX"      "005215364" "30383192" "518264"
  # [22,] "VT"      "007251031" "43285273" "563454"
  # [23,] "VA"      "009214076" "37170132" "579064"
  # [24,] "PR"      "006062019" "18143610" "612791"
  # [25,] "PR"      "006552085" "18174528" "613249"
  # [26,] "PR"      "006641714" "18252260" "614141"
  # [27,] "PR"      "006634092" "18124803" "614154"
  # [28,] "PR"      "006601080" "18002070" "614309"
  # [29,] "PR"      "000664354" "18101250" "614367"
  #
  # It looks like most just have one or two extra leading zeros. 
  # Some are missing the first digit.
  
  # > rawDF[485978:485983, 20:22]
  # # A tibble: 6 × 3
  # LAT_016  LONG_017 DETOUR_KILOS_019
  # <chr>     <chr>            <chr>
  #   1 44035086 103400535               12
  # 2 44035232 103421805               12
  # 3 44041556 003421197               12
  # 4 44040888 003431496               12
  # 5 00000000 000000000               12
  # 6 44040000 103283000                0
  #
  # Should start with "1"
  lonDMS[485980:485981] <- paste0("1", stringr::str_sub(lonDMS[485980:485981], 2,9))
  
  # > rawDF[38490:38495, 20:21]
  # # A tibble: 6 × 2
  # LAT_016  LONG_017
  # <chr>     <chr>
  #   1 34275700 094154534
  # 2 34253432 094142184
  # 3 34251204 004090000
  # 4 34435628 094165988
  # 5 34434404 094181381
  # 6 34394104 093594416
  #
  # Should start with "09"
  lonDMS[38492] <- paste0("09", stringr::str_sub(lonDMS[38492], 3,9))
  
  # > rawDF[337118:337123, 20:21]
  # # A tibble: 6 × 2
  # LAT_016  LONG_017
  # <chr>     <chr>
  #   1 43550012 071315808
  # 2 00000000 000000000
  # 3 44025294 001110938
  # 4 42554390 071374620
  # 5 42561310 071381350
  # 6 42561850 071382730 
  lonDMS[337120] <- paste0("07", stringr::str_sub(lonDMS[337120], 3,9))
  
  # > rawDF[69485:69490, 20:21]
  # # A tibble: 6 × 2
  # LAT_016  LONG_017
  # <chr>     <chr>
  #   1 38502047 104422489
  # 2 38494744 000003883
  # 3 38494740 104431795
  # 4 38494884 104431291
  # 5 38502047 104425655
  # 6 38590203 104322268
  lonDMS[69486]  <- NA
  
  # > rawDF[337118:337123, 20:21]
  # # A tibble: 6 × 2
  # LAT_016  LONG_017
  # <chr>     <chr>
  #   1 43550012 071315808
  # 2 00000000 000000000
  # 3 44025294 001110938
  # 4 42554390 071374620
  # 5 42561310 071381350
  # 6 42561850 071382730
  lonDMS[383158] <- paste0("08", stringr::str_sub(lonDMS[383158], 3,9))
  
  # > rawDF[518260:518266, 20:21]
  # # A tibble: 7 × 2
  # LAT_016  LONG_017
  # <chr>     <chr>
  # 2 31233782 095093343
  # 3 31173692 095071913
  # 4 30305851 095042675
  # 5 30383192 005215364
  # 6 31314692 094012287
  lonDMS[518264] <- paste0("09", stringr::str_sub(lonDMS[518264], 3,9))
  
  # The rest (non IA or MD) low longitudes have one or two extra leading zeros.
  lowLonNotiamd <- !is.na(lonDMS) & as.numeric(lonDMS) < 1e7 & nbi$stateCode != "MD" & nbi$stateCode != "IA"
  lonDMS[lowLonNotiamd] <-
    ifelse(as.numeric(lonDMS[lowLonNotiamd]) < 2e6, paste0(stringr::str_sub(lonDMS[lowLonNotiamd], 3,9), "00"),
           paste0(stringr::str_sub(lonDMS[lowLonNotiamd], 2,9), "0"))
  
  badIA <- !is.na(lonDMS) & as.numeric(lonDMS) < 1e7 & nbi$stateCode == "IA"
  # > summary(as.numeric(lonDMS)[badIA])
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 911400  932700  934200  936300  944900  954800 
  # 
  # low longitudes in IA consitently are lacking two 0's at the end. 
  lonDMS[badIA] <- paste0(stringr::str_sub(lonDMS[badIA], 3,9), "00")
  
  # all remaining low longitudes are in MD. It appears that and 0s in MD longitudes were put at the 
  # beginning of the string instead of the end. 
  # > stringr::str_length(as.numeric(lonDMS[lowLonMask])) %>% table
  # .
  # 2    4    5    6    7 
  # 2    2   22 1890   16 
  
  lowLonMask <- !is.na(lonDMS) & as.numeric(lonDMS) < 1e7
  badLons <- as.numeric(lonDMS[lowLonMask])
  
  lonDMS[lowLonMask] <- 
    ifelse(badLons > 1e6 , paste0(stringr::str_sub(lonDMS[lowLonMask], 2,9), "0"),
           ifelse(badLons > 1e5, paste0(stringr::str_sub(lonDMS[lowLonMask], 3,9), "00"),
                  ifelse(badLons > 1e4, paste0(stringr::str_sub(lonDMS[lowLonMask], 4,9), "000"),
                         ifelse(badLons > 1e3, paste0(stringr::str_sub(lonDMS[lowLonMask], 5,9), "0000"),
                                paste0(stringr::str_sub(lonDMS[lowLonMask], 7,9), "000000")))))
  
  lowLonMask <- lowLonMask <- !is.na(lonDMS) & as.numeric(lonDMS) < 6.5e7
  # NOTE:  Many rows have longitude = "021570000", latitude = "64100000". 
  # Not sure what it means.
  # weirdlons <- !is.na(lonDMS) & lonDMS == "021570000"
  # cbind(stateCode = nbi$stateCode[weirdlons], longitude = lonDMS[weirdlons], latitude = latDMS[weirdlons])
  # stateCode longitude   latitude  
  # [1,] "AL"      "021570000" "64100000"
  # [2,] "AL"      "021570000" "64100000"
  # [3,] "AL"      "021570000" "64100000"
  # [4,] "AL"      "021570000" "64100000"
  # [5,] "AL"      "021570000" "64100000"
  # [6,] "AL"      "021570000" "64100000"
  # [7,] "CO"      "021570000" "64100000"
  # [8,] "NJ"      "021570000" "40243053"
  # [9,] "ND"      "021570000" "64100000"
  # [10,] "ND"      "021570000" "64100000"
  # [11,] "ND"      "021570000" "64100000"
  # [12,] "ND"      "021570000" "64100000"
  # [13,] "ND"      "021570000" "64100000"
  # [14,] "ND"      "021570000" "64100000"
  # [15,] "ND"      "021570000" "64100000"
  # [16,] "ND"      "021570000" "64100000"
  #   ...
  lonDMS[!is.na(lonDMS)&lonDMS=="021570000"] <- NA
  latDMS[!is.na(latDMS)&latDMS == "64100000"] <- NA
  
  
  
  #######################################################
  
  # NOTE:  these are degrees W
  
  deg <- as.numeric(stringr::str_sub(lonDMS, 1, 3))
  min <- as.numeric(stringr::str_sub(lonDMS, 4, 5))
  sec <- as.numeric(stringr::str_sub(lonDMS, 6, 9))/100
  
  nbi$longitude <- -1 * (deg + min/60 + sec/3600)
  
  # Remove values that are out of domain
  
  badMask <- nbi$longitude < -180 | nbi$longitude > -65
  nbi$longitude[badMask] <- NA
  
  # Remove remaining problematic lats and lons
  bad1 <- !is.na(nbi$longitude) & !is.na(nbi$latitude) & nbi$latitude > 50 & nbi$longitude > -129
  nbi$latitude[bad1] <- NA
  nbi$longitude[bad1] <- NA
  bad2 <- !is.na(nbi$longitude) & !is.na(nbi$latitude) & nbi$latitude >27.2 & nbi$latitude < 32.5 & nbi$longitude > -79
  nbi$latitude[bad2] <- NA
  nbi$longitude[bad2] <- NA
  
  
  
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
  
  nbi$channelCondition <- as.numeric(rawDF$CHANNEL_COND_061)
  
  return(nbi)
  
}
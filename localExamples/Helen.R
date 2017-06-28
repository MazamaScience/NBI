#  Helen's explorations in bridges

# Ingest data

source("R/convert2016.R")
nbi <- convert2016("~/Data/2016hwybronlyonefile.zip")


nbi$age <- 2016 - nbi$yearBuilt
meanAgeByState <- aggregate(data = nbi, age ~ stateCode, FUN = mean)
medianAgeByState <- aggregate(data = nbi, age ~ stateCode, FUN = median)
modeAgeByState <- aggregate(data = nbi, age ~ stateCode , FUN = mode)

orderedAge <- meanAgeByState[order(meanAgeByState$age),]
orderedMedian <- medianAgeByState[order(medianAgeByState$age),]

barplot(orderedAge$age, horiz=TRUE, names.arg=orderedAge$stateCode, las=1, xlim=c(0,60))
abline(v=seq(0,80,5), col='white', lty='dashed')
title("Mean Bridge Age by State")

barplot(orderedMedian$age, horiz=TRUE, names.arg=orderedMedian$stateCode, las=1, xlim=c(0,60))
abline(v=seq(0,80,20), col='white', lty='dashed')
title("Median Bridge Age by State")

# Make a plot of Continental US with states colored by mean bridge age
# 1. create a color index mask for meanAgeByState using .bincode
stateBreaks <- seq(30,60,by=5)
meanAgeIndex <- .bincode(meanAgeByState$age, breaks = stateBreaks)

# 2. create color palette
stateColors <- RColorBrewer::brewer.pal(6,"PuRd")

# 3. get state names associated with stateCodes
df <- maps::state.fips
df$stateName <- stringr::str_extract(df$polyname, "[^:]+")
df1 <- df[!duplicated(df$stateName), ]
nameByCode <- as.character(df1$stateName)
names(nameByCode) <- as.character(df1$abb)

names(meanAgeIndex) <- meanAgeByState$stateCode


# # 4. plot states, filled in by color index, corresponding to the color pallete from part 2.
#  # For some reason, the maps package does not color the states correctly...
#  map("state", nameByCode[meanAgeByState$stateCode], fill = TRUE, col = stateColors[meanAgeIndex])
#  legend("bottomleft", legend = stateBreaks, pch = 15, col = stateColors[1:8])

#Now, do the same thing but with MazamaSpatialUtils
library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("NaturalEarthAdm1")
us <- subset(NaturalEarthAdm1, countryCode == "US")
conusID <- us$stateCode
conusID <- setdiff(conusID, c("AK", "HI"))
conus <- subset(us, stateCode %in% conusID)

plot(conus, col = stateColors[meanAgeIndex[conus$stateCode]])
legend("bottomleft", legend = c("30-35 yrs", "35-40 yrs", "40-45 yrs", "45-50 yrs", "50-55 yrs", "55-60 yrs"), 
       pch = 15, col = stateColors[1:6], title = "Mean Bridge Age")
title("Mean Bridge Age")
 
#In which decade were most bridges built in each state?
getmode <- function(v) {
uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]
}
modeAgeByState <- aggregate(data = nbi, yearBuilt ~ stateCode , FUN = getmode)



decadeBreaks <- c(-Inf, seq(1800, 2020, by = 10))
decadeBuilt <- .bincode(nbi$yearBuilt, breaks = decadeBreaks)
decadeBuilt <- c("<1800", as.character(seq(1800,2020,by = 10)))[decadeBuilt]
modeDecadeByState <- aggregate(decadeBuilt ~ nbi$stateCode , FUN = getmode)
modeDecadeByState$decadeBuilt <- as.numeric(modeDecadeByState$decadeBuilt)
modeAgeIndex <- .bincode(modeDecadeByState$decadeBuilt, breaks = seq(1929,1999, by = 10))
names(modeAgeIndex) <- modeDecadeByState$`nbi$stateCode`
decadeColors <- brewer.pal(7, "PuRd")

plot(conus, col = decadeColors[modeAgeIndex[conus$stateCode]])
legend("bottomleft", c("1930s", "1940s", "1950s", "1960s", "1970s", "1980s", "1990s"), pch = 15, col = decadeColors)



#############################################################################

decadeBreaks <- c(-Inf, seq(1800, 2020, by = 10))
decadeBuilt <- .bincode(nbi$yearBuilt, breaks = decadeBreaks)
decadeBuilt <- c("<1800", as.character(seq(1800,2020,by = 10)))[decadeBuilt]
modeDecadeByState <- aggregate(decadeBuilt ~ nbi$stateCode , FUN = getmode)
modeDecadeByState$decadeBuilt <- as.numeric(modeDecadeByState$decadeBuilt)
modeAgeIndex <- .bincode(2000-modeDecadeByState$decadeBuilt, breaks = seq(9,79, by = 10))
names(modeAgeIndex) <- modeDecadeByState$`nbi$stateCode`
decadeColors <- RColorBrewer::brewer.pal(7, "PuRd")

plot(conus, col = decadeColors[modeAgeIndex[conus$stateCode]])
legend("bottomleft", c("1930s", "1940s", "1950s", "1960s", "1970s", "1980s", "1990s"), pch = 15, col = decadeColors[7:1])



#############################################################################
# Make a plot of Pennsylvania with bridges colored by year

#Index years so bridges can be colored by year made
breaks <- c(-Inf, seq(1900, 2020, by=20))
colorIndex <- .bincode(nbi$yearBuilt, breaks=breaks)
nbi$colorIndex <- colorIndex
#Define colors for each year
library(RColorBrewer) 
myColors <- brewer.pal(7, "Spectral")
#draw the map
library(maps)
map("state", "Penn")
pa <- subset(nbi, stateCode == "PA")
points(pa$longitude, pa$latitude, pch=17, cex=(3.5-pa$colorIndex/2), col=myColors[pa$colorIndex])
map("county", add=TRUE)
legend("topright", horiz=TRUE, legend  = breaks, fill = myColors)

#Where are the oldest bridges?
oldPa <- subset(pa, yearBuilt <= 1900)
map("state", "Penn")
points(oldPa$longitude, oldPa$latitude, pch = 17, cex = .5)

#Where are the newest bridges?
newPa <- subset(pa, yearBuilt >= 2000)
map("state", "Penn")
points(newPa$longitude, newPa$latitude, pch = 17, cex = .5)


###########

# nbi$designLoad <- rawDF[,"DESIGN_LOAD_031"]

###########

# make a map where bridges are sized based on average daily traffic

map("state")
points(nbi$longitude, nbi$latitude, pch = 17, cex = as.numeric(nbi$averageCarCount)/807000*6)
title("Bridge Traffic")
#legend("bottomleft", legend = c("10", "100", "1000", "10000", "100000"), pch = rep(17, 5), 
# cex = c(10, 100, 1000, 10000, 100000)/807000*6)

map("state", "north Dakota")


#Aggregate some of the traffic data by state

totalDailyTrafficByState <- aggregate(data = nbi, averageCarCount~stateCode, FUN = sum)
orderedTraffic <- totalDailyTrafficByState[order(totalDailyTrafficByState$averageCarCount),]

trafficDensityByState <- aggregate(data = nbi, averageCarCount~stateCode, FUN = mean)
orderedTrafficDensity <- trafficDensityByState[order(totalDailyTrafficByState$averageCarCount),] #ordered the same for comparison

nBridgesByState <- aggregate(data = nbi , averageCarCount~stateCode, FUN = length)
orderedBridgesByState <- nBridgesByState[order(totalDailyTrafficByState$averageCarCount),] #ordered the same for comparison

par(mfrow = c(1,3))
barplot(orderedTraffic$averageCarCount, horiz=TRUE, names.arg=orderedTraffic$stateCode, las=1, main = "Total Bridge Crossings per day")
barplot(orderedBridgesByState$averageCarCount, horiz = TRUE, las = 1, main = "Number of Bridges")
barplot(orderedTrafficDensity$averageCarCount, horiz = TRUE ,  las = 1, main = "Traffic Density")




#Multi-lane bridges
# summary(nbi$laneCount)
# summary(nbi$underLaneCount)
# #most bridges have two lanes and pass over 0 lanes.
# #what is up with bridges with 82 lanes, or passing over 99 lanes? 
# 
# table(nbi$laneCount)
# table(nbi$underLaneCount)
# 
# bigBridges <- nbi$laneCount > 8
# reallyBigBridges <- nbi$laneCount > 20 

# map("state")
# points(nbi$longitude[bigBridges], nbi$latitude[bigBridges], cex = nbi$laneCount[bigBridges]/15, pch = 2)
# #Big bridges tend to be really clumped together. Most likely around big cities.
# map("state")
# points(nbi$longitude[reallyBigBridges], nbi$latitude[reallyBigBridges], cex = nbi$laneCount[reallyBigBridges]/25, pch = 2)
# #What is going on in Tennessee?
# 
# #It looks like big bridges might follow similar patterns to the high-traffic bridges. Let's see.
# map("state")
# points(nbi$longitude, nbi$latitude, pch = 17, cex = as.numeric(nbi$averageCarCount)/807000*6)
# points(nbi$longitude[bigBridges], nbi$latitude[bigBridges], cex = nbi$laneCount[bigBridges]/15, pch = 2, col = "red")
# 
# 
# #There are a lot of really HUGE bridges in TN? Why? Let's see what's going on there. 
# tn <- dplyr::filter(nbi, stateCode == "TN")
# map("state", "ten")
# points(tn$longitude, tn$latitude, pch = 17, cex = as.numeric(tn$averageCarCount)/807000*6)
# points(nbi$longitude[bigBridges], nbi$latitude[bigBridges], cex = nbi$laneCount[bigBridges]/15, pch = 2, col = "red")



MazamaSpatialUtils::convertUSCensusCounties()
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusCounties")

nbiWa <- subset(nbi, stateCode == "WA")

nbiWa$county <- getUSCounty(nbiWa$longitude, nbiWa$latitude, dataset = "USCensusCounties", stateCode = "WA")
aggregate(data = nbiWa, age~county, FUN = "mean") -> meanWaAge

breaks <- seq(30, 60, by = 5)
colorIndex <- .bincode(meanWaAge$age, breaks=breaks)
names(colorIndex) <- meanWaAge$county

wa <- subset(USCensusCounties, stateCode == "WA")

plot(wa, col = stateColors[colorIndex[wa$countyName]])
legend("bottomleft", c("30-35 yrs", "35-40 yrs", "40-45 yrs", "45-50 yrs", "50-55 yrs","55-60 yrs"), 
       pch = 15, col = stateColors[1:6], title = "Mean Bridge Age")

bridgeCount <- aggregate(data = nbiWa, age~county, FUN = "length")
breaks <- seq(0,1200, by = 200)
colorIndex <- .bincode(bridgeCount$age, breaks = breaks)
names(colorIndex) <- bridgeCount$county

plot(wa, col = stateColors[colorIndex[wa$countyName]])
legend("bottomleft", c("0-200", "201-400", "401-600", "601-800", "801-1,000", "1,001-1,200"), 
       pch = 15, col = stateColors[1:6], title = "Number of Bridges")
waCities <- subset(us.cities, country.etc == "WA")
points(waCities$long, waCities$lat, pch = 1, cex = waCities$pop/80000, col = "red")
points(nbiWa$longitude, nbiWa$latitude, pch = 2, cex = nbiWa$averageCarCount/20000)

points(nbiWa[as.logical(nbiWa$water-1),]$longitude, nbiWa[as.logical(nbiWa$water-1),]$latitude, pch = 2, 
       cex = nbiWa[as.logical(nbiWa$water-1),]$averageCarCount/20000)
points(nbiWa[as.logical(nbiWa$water),]$longitude, nbiWa[as.logical(nbiWa$water),]$latitude, pch = 2, 
       cex = nbiWa[as.logical(nbiWa$water),]$averageCarCount/20000)

points(nbiWa$longitude, nbiWa$latitude, cex = nbiWa$roadWidth/40, pch = 2)


map("state")
with(dplyr::filter(nbi, water == 0), points(longitude, latitude, pch = 17, cex = .1))
map("state")
with(dplyr::filter(nbi, water == 1), points(longitude, latitude, pch = 17, cex = .01))


map("state", "neb")
with(dplyr::filter(nbi, water == 1 & stateCode == "NE"), points(longitude, latitude, pch = 2, cex = .2))
with(dplyr::filter(nbi, water == 0 & stateCode == "NE"), points(longitude, latitude, pch = 2, cex = .5, col = "red"))




map("state", "minne")
with(dplyr::filter(nbi, water == 0 & stateCode == "MN"), points(longitude, latitude, pch = 2, cex = .1))
map("state", "minne")
with(dplyr::filter(nbi, water == 1 & stateCode == "MN"), points(longitude, latitude, pch = 2, cex = .1))



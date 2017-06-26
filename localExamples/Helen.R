#  Helen's explorations in bridges

# Ingest data

source("R/convert2016.R")
nbi <- convert2016("~/Data/2016hwybronlyonefile.zip")


nbi$age <- 2016 - nbi$yearBuilt
meanAgeByState <- aggregate(data = nbi, age ~ stateCode, FUN = mean)
medianAgeByState <- aggregate(data = nbi, age ~ stateCode, FUN = median)

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

# nbi$averageCarCount <- as.numeric(rawDF$ADT_029)
# nbi$designLoad <- rawDF[,"DESIGN_LOAD_031"]
 nbi$waterway <- rawDF[,"WATERWAY_EVAL_071"]
 nbi$water <- ifelse(nbi$Waterway == "N", 0, 1)

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
summary(nbi$laneCount)
summary(nbi$underLaneCount)
#most bridges have two lanes and pass over 0 lanes.
#what is up with bridges with 82 lanes, or passing over 99 lanes? 

table(nbi$laneCount)
table(nbi$underLaneCount)

bigBridges <- nbi$laneCount > 8
reallyBigBridges <- nbi$laneCount > 20 

map("state")
points(nbi$longitude[bigBridges], nbi$latitude[bigBridges], cex = nbi$laneCount[bigBridges]/15, pch = 2)
#Big bridges tend to be really clumped together. Most likely around big cities.
map("state")
points(nbi$longitude[reallyBigBridges], nbi$latitude[reallyBigBridges], cex = nbi$laneCount[reallyBigBridges]/25, pch = 2)
#What is going on in Tennessee?

#It looks like big bridges might follow similar patterns to the high-traffic bridges. Let's see.
map("state")
points(nbi$longitude, nbi$latitude, pch = 17, cex = as.numeric(nbi$averageCarCount)/807000*6)
points(nbi$longitude[bigBridges], nbi$latitude[bigBridges], cex = nbi$laneCount[bigBridges]/15, pch = 2, col = "red")


#There are a lot of really HUGE bridges in TN? Why? Let's see what's going on there. 
tn <- dplyr::filter(nbi, stateCode == "TN")
map("state", "ten")
points(tn$longitude, tn$latitude, pch = 17, cex = as.numeric(tn$averageCarCount)/807000*6)
points(nbi$longitude[bigBridges], nbi$latitude[bigBridges], cex = nbi$laneCount[bigBridges]/15, pch = 2, col = "red")

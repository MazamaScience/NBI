#  Helen's explorations in bridges

# Ingest data

source("R/convert2016.R")
nbi <- convert2016("~/Data/2016hwybronlyonefile.zip")

#When were bridges built?
hist(nbi$yearBuilt, main = "Year Built",n=50)

#Center it around 1900
nbi$yearSince1900 <- nbi$yearBuilt-1900

#How do bridge ages differ between different states?
yearBuiltMeans <- aggregate(data = nbi, yearSince1900~stateCode, FUN = mean)
yearBuiltMedians <- aggregate(data = nbi, yearSince1900~stateCode, FUN = median)
#How can I visualize these results?

# Make a plot of Pennsylvania with bridges colored by year

#Index years so bridges can be colored by year made
breaks <- c(-Inf, seq(1900, 2020, by=20))
colorIndex <- .bincode(nbi$yearBuilt, breaks=breaks)
nbi$colorIndex <- colorIndex
#Define colors for each year
library(RColorBrewer) 
myColors <- brewer.pal(7, "Spectral")
#draw the map
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





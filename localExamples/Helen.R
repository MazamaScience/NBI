#  Helen's explorations in bridges

# Ingest data

source("R/convert2016.R")
nbi <- convert2016("~/Data/2016hwybronlyonefile.zip")
nbi$yearBuilt <- as.numeric(rawDF$YEAR_BUILT_027)

# Make a plot of Pennsylvania with bridges colored by year

#Divide year made into groups
breaks <- c(-Inf, seq(1900, 2020, by=20))
colorIndex <- .bincode(nbi$yearBuilt, breaks=breaks)
#Define colors for ages 
library(RColorBrewer) 
myColors <- brewer.pal(7, "Spectral")
#draw the map
map("state", "Penn")
pa <- subset(nbi, stateCode == "PA")
points(pa$longitude, pa$latitude, pch=17, cex=.5, col=myColors[colorIndex])
map("county", add=TRUE)
legend("topright", horiz=TRUE, legend  = breaks, fill = myColors)



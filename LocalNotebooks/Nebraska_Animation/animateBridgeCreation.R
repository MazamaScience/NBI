
library(animation)



# #Nice animation using Hadley tools
# 
# source("R/convert2016.R")
# nbi <- convert2016("~/Data/2016hwybronlyonefile.zip")
# 
# library(tidyverse)
# 
# ne <- subset(nbi, stateCode == "NE")
# nested <- nest(group_by(ne, yearBuilt))
# nested <- nested[order(nested$yearBuilt), ]
# 
# 
# bridgeDraw <- function(df, year){
#   maps::map("state", "neb")
#   points(df$longitude, df$latitude, pch = 17)
#   title(as.character(year))
# }
# 
# saveHTML(purrr::map2(nested$data, nested$yearBuilt, bridgeDraw), htmlfile = "nebraska.html", autobrowse = FALSE)


### Function which will show where bridges were built each year in a particular state by creating an animation

animateBridgeCreation <- function(df,  htmlfile, state, yearRange = NULL){

    
  ##Define plotting function
  yearlyBridgePlot <- function(df, year, state = ""){
    layout(matrix(c(1,2), 2, 1), heights = c(7,3))
    par(mar = c(2,2,2,2))
    maps::map("state", state)
    with(subset(df, yearBuilt <= year - 10), 
         points(longitude, latitude, cex = .3, pch = 17, col = "grey"))
    for(i in 1:9){
      with(subset(df, yearBuilt == year - i),
           points(longitude, latitude, cex = .4, pch = 17, 
                  col = adjustcolor("grey", red.f = 2-i/10, green.f = i/10, blue.f = i/10)))
    }
    with(subset(df, yearBuilt == year), points(longitude, latitude, pch = 24, cex = 1, bg = "red", col = "black"))
    title(as.character(year))
    par(mar = c(1,4,5,4), mgp = c(2,.5,0), yaxs = "i")
    plot(NA, xlim = c(1885, 2015), ylim = c(0,10), axes = F, ann = F)
    axis(3, at = seq(1885, 2015, by = 10))
    points(year, 8.8, pch = 17, cex = 2)
    title("Year")
  }
  
  stateDf <- subset(df, stateCode == state)
  
  ##Get state name for mapping
  
  fipDf <- maps::state.fips[,c(5,6)]
  fipDf$stateName <- stringr::str_extract(fipDf$polyname, "[^:]+")
  fipDf1 <- fipDf[!duplicated(fipDf$stateName), ]
  nameByCode <- as.character(fipDf1$stateName)
  names(nameByCode) <- as.character(fipDf1$abb)
  
  ##make the animation
  
    if(is.null(yearRange)){
      animation::saveHTML(for(i in min(stateDf$yearBuilt):max(stateDf$yearBuilt)){
        yearlyBridgePlot(df = stateDf, year = i, state = nameByCode[state])
      }, 
      interval = .2, htmlfile = htmlfile, autobrowse = TRUE)
      }else{
        animation::saveHTML(
        for(i in yearRange){
          yearlyBridgePlot(df = stateDf, year = i, state = state)
        },
      interval = .2,
      htmlfile = htmlfile, autobrowse = TRUE)
      }
}

# #Example:
# animateBridgeCreation(nbi, "stateanimation.html", state = c("WA", "OR", "CA"))

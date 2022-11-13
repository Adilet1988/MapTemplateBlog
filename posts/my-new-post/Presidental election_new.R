##################################################
## Kyrgyzstan Election Data Visualisation       ##
## Author: Adilet Bekturov                      ##
## Date: 2018-05-12                             ##
## Comments: code to generate maps of KGZ       ##
##################################################

## Define the directory
getwd()

## Change the directory
setwd("C:\Users\admin\Documents\GitHub\MapBlog\posts\my-new-post")

## install packages
install.packages("tmap")
install.packages("leaflet")
install.packages("magrittr")
install.packages("rio")
install.packages("plyr")
install.packages("scales")
install.packages("htmlwidgets")
install.packages("rgdal")
install.packages("tmaptools")

---
title: "Map of Kyrgyzstan Presidental Election (2017)"
author: "Adilet Bekturov"
output: html_document
---
  

## activate packages
library("tmap")
library("leaflet")
library("magrittr")
library("rio")
library("plyr")
library("scales")
library("htmlwidgets")
library("ggplot2")
library("rgdal")
library("tmaptools")


## Open the shp file
mngeo <- readOGR("C:/Users/admin/Documents/GitHub/MapBlog/posts/my-new-post/kg_rayon.geojson")

mngeobins <- c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 450000)
mngeopal <- colorBin("YlGnBu", domain = mngeo$voters, bins = mngeobins)


## Give up name
mnpopup <- paste0("<b>","Rayon: ","<b>", mngeo@data$NAME, "<br>",
                  "<b>","Code: ","<b>", mngeo@data$NAME_A2, "<br>",
                  "<b>","Total voters: ","<b>", mngeo@data$voters, "<br>",
                  "<b>","Zarlykov: ","</b>", mngeo@data$Zarlykov_E, "<br>",
                  "<b>","Umetalieva: ","</b>", mngeo@data$Umetalieva, "<br>",
                  "<b>","Sariev: ","</b>", mngeo@data$Sariev_Tem, "<br>",
                  "<b>","Kochkorov: ","</b>", mngeo@data$Kochkorov_, "<br>",
                  "<b>","Masadykov: ", "</b>", mngeo@data$Masadykov_, "<br>",
                  "<b>","Babanov: ", "</b>", mngeo@data$Babanov_Om, "<br>",
                  "<b>","Abdyldaev: ", "</b>", mngeo@data$Abdyldaev_, "<br>",
                  "<b>","Beknazarov: ","</b>", mngeo@data$Beknazarov, "<br>",
                  "<b>","Zheenbekov: ", "</b>", mngeo@data$Zheenbekov, "<br>",
                  "<b>","Madumarov: ","</b>", mngeo@data$Madumarov_, "<br>",
                  "<b>","Maliev: ","</b>", mngeo@data$Maliev_Ars, "<br>",
                  "<b>","Against to all: ","</b>", mngeo@data$Against_ev)

## Show the map
leaflet(mngeo) %>%
  addProviderTiles("OpenStreetMap") %>%
  addPolygons(stroke=TRUE, 
              smoothFactor = 0.2,
              weight = 1,
              fillOpacity = .6, 
              popup=mnpopup,
              color= ~mngeopal(mngeo@data$voters)
  )%>% addLegend("bottomright", pal = mngeopal, values = ~mngeo@data$voters, title = "Results",
                 labFormat = labelFormat(suffix = ' voters', between = '-',
                                         transform = function(x) 1 * x))



#section for multi-layered
minPct <- min(c(mngeo@data$voters, mngeo@data$ballot_pap, mngeo@data$Zarlykov_E, mngeo@data$Umetalieva, mngeo@data$Sariev_Tem, mngeo@data$Kochkorov_, mngeo@data$Masadykov_, mngeo@data$Babanov_Om, mngeo@data$Abdyldaev_, mngeo@data$Beknazarov, mngeo@data$Zheenbekov, mngeo@data$Madumarov_, mngeo@data$Maliev_Ars, mngeo@data$Against_ev))
maxPct <- max(c(mngeo@data$voters, mngeo@data$ballot_pap, mngeo@data$Zarlykov_E, mngeo@data$Umetalieva, mngeo@data$Sariev_Tem, mngeo@data$Kochkorov_, mngeo@data$Masadykov_, mngeo@data$Babanov_Om, mngeo@data$Abdyldaev_, mngeo@data$Beknazarov, mngeo@data$Zheenbekov, mngeo@data$Madumarov_, mngeo@data$Maliev_Ars, mngeo@data$Against_ev))
paletteLayers <- colorBin(palette = "RdBu", domain = c(minPct, maxPct), bins = c(500000, 200000, 100000, 50000, 40000, 30000, 20000, 10000, 5000, 1000, 500, 0) , pretty=FALSE)
incumbPalette <- colorFactor(palette = c("red", "grey", "blue"), domain = mngeo@data$voters)

## Show the map
leaflet(mngeo) %>%
  addProviderTiles("OpenStreetMap") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=mnpopup, 
              color= ~mngeopal(mngeo@data$voters),
              group="Number of voters"
  ) %>%
  addLegend(position="bottomleft", colors=c('red', 'grey', 'blue'), labels=c("Low", "Medium", "High"))

## Create a online map
mnResultmap <- leaflet(mngeo) %>% 
  addProviderTiles("OpenStreetMap") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=mnpopup, 
              color= ~mngeopal(mngeo@data$voters),
              group="Number of voters"
  ) %>% 
  addLegend(position="bottomleft", colors=c('red', 'grey', 'blue'), labels=c("Low", "Medium", "High"))  %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~mngeopal(mngeo@data$ballot_pap),
              group="Number of ballot papers"
  ) %>% addLegend("bottomright", pal = paletteLayers, values = ~mngeo@data$ballot_pap, title = "Results",
                  labFormat = labelFormat(suffix = ' voter', between = '-',
                                          transform = function(x) 1 * x)) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mngeo@data$Zarlykov_E),
              group="Zarlykov E.Z."
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mngeo@data$Umetalieva),
              group="Umetalieva T.Zh."
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mngeo@data$Sariev_Tem),
              group="Sariev T.A."
  ) %>%  

  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mngeo@data$Kochkorov_),
              group="Kochkorov U.T."
  ) %>%  
    
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mngeo@data$Masadykov_),
              group="Masadykov T.Sh."
  ) %>%

  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mngeo@data$Babanov_Om),
              group="Babanov O.T."
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mngeo@data$Abdyldaev_),
              group="Abdyldaev A.B."
  ) %>%
 
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mngeo@data$Beknazarov),
              group="Beknazarov A.A."
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mngeo@data$Zheenbekov),
              group="Zheenbekov S. Sh."
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mngeo@data$Madumarov_),
              group="Madumarov A.K."
  ) %>%
 
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mngeo@data$Maliev_Ars),
              group="Maliev A.K."
  ) %>%

  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mngeo@data$Against_ev),
              group="Against to all"
  ) %>%
  
  addLayersControl(
    baseGroups=c("Number of voters", "Number of ballot papers", "Zarlykov E.Z.", "Umetalieva T.Zh.", "Sariev T.A.", "Kochkorov U.T.", "Masadykov T.Sh.", "Babanov O.T.", "Abdyldaev A.B.", "Beknazarov A.A.", "Zheenbekov S. Sh.", "Madumarov A.K.", "Maliev A.K.", "Against to all"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  ) 
## Show finish result
mnResultmap

## Save the result
saveWidget(mnResultmap, file="index.html")


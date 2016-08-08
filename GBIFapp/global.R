library(shiny)
library(shinythemes)
# library(shinyBS)
library(leaflet)
library(dplyr)

library(rgdal)
require(raster)
library(maptools)

library(RColorBrewer)
library(scales)
library(lattice)


YearsRng<-c(1741,2016) #because I know! range(unique(AmpEur$year), na.rm = T)
Years<-c(NA, seq(YearsRng[1],YearsRng[2]))
YearsPlot<-Years
YearsPlot[1]<-Years[2]-1

AmpEur100 <- raster("data/AmpEur100.tif")
AmpEur50 <- raster("data/AmpEur50.tif")
AmpEur25 <- raster("data/AmpEur25.tif")

AmpEurR100 <- raster("data/AmpEurR100.tif")
AmpEurR50 <- raster("data/AmpEurR50.tif")
AmpEurR25 <- raster("data/AmpEurR25.tif")

Rana100<-raster("data/Rana100.tif")
Rana50<-raster("data/Rana50.tif")
Rana25<-raster("data/Rana25.tif")

load("data/AmpEur100Stacks.rData")

RanaPoly<-readShapePoly("data/species_58734/species_58734simp.shp", proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

CountEurope<-readShapePoly("data/countries Europe.shp", proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
CountEurope<-spTransform(CountEurope, CRS("+init=epsg:3857"))
Countries<-as.character(CountEurope@data$CNTRY_NAME)
CountriesAb<-Countries
CountriesAb[43]<-"Bos. & Herz."
CountriesAb[29]<-"Isle of Man"

CountriesList<-Countries[order(CountriesAb)]
CountriesListAb<-CountriesAb[order(CountriesAb)]
CountriesNumbers<-c(1:length(Countries))[order(CountriesAb)]





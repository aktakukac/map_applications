## Source

setwd("C:/Garamvolgyi/Munka/Solar Lead")

library(sp)
library(xlsx)
library(data.table)
library(reshape2)
library(raster)
library(ggmap)
library(mapproj)
library(ggplot2)
library(rgdal)
library(maptools)
library(RColorBrewer)

set.seed(357)

## reads the data table for sales region coordinates from xls and converts to data table

SalesRegionCoordinates <- read.xlsx(
							  "C:/Garamvolgyi/Munka/Solar Lead/Sales_region_v2.xls", 
							  1,  
							  as.data.frame=TRUE, 
							  header=TRUE, 
							  colClasses=NA,
							  keepFormulas=FALSE, 
							  encoding="unknown"
)

SalesRegionCoordinates <- data.table(SalesRegionCoordinates)
setkey(SalesRegionCoordinates, Sales_region)

## gets the sales region names from file
get_levels <- function(data,fac){
  levels(data[[fac]])
}
SalesRegionNames <- get_levels(SalesRegionCoordinates, 'Sales_region')

## reads city coordinates 
CityCoordinates <- read.csv	(
								"C:/Garamvolgyi/Munka/Solar Lead/Helyseg_koordinatak.csv", 
								header=TRUE, 
								sep=";", 
								dec=","
)

CityCoordinatesOriginal <- data.table(CityCoordinates)
CityCoordinates <- data.table(CityCoordinates)

## creates an empty result file
SalesMatrix <- data.table(CityCoordinatesOriginal)

## lookup for all sales regions

print("starting loop!")

for (i in seq_along(SalesRegionNames)) { ## seq_along is for vectors
  
		  print(i)
  
      SalesMatrix <- SalesMatrix[ , SalesRegionNames[i]:= 0]
		  
		  CurrentSalesRegionCoordinates <- SalesRegionCoordinates[SalesRegionNames[i]]
		  
		  coordinates(CurrentSalesRegionCoordinates) <- ~ Hosszusag + Szelesseg
		  
		  ## spatial polygon konverzió
		  CurrentSalesRegionCoordinates <- SpatialPolygons(
				list(Polygons(list(Polygon(CurrentSalesRegionCoordinates)), ID = 1))
				)
		  
		  for (j in seq_len(nrow(CityCoordinates))) { ## seq_len is for data frames
			
				CurrentCityCoordinate <- CityCoordinates[j]  
				
				coordinates(CurrentCityCoordinate) <- ~ HOSSZUSAG + SZELESSEG
				
				CurrentCityCoordinate <- SpatialPoints(CurrentCityCoordinate)
				
				OV <- over(CurrentCityCoordinate,CurrentSalesRegionCoordinates)
				
				if (!is.na(OV)) {
				  
					SalesMatrix <- SalesMatrix[j , SalesRegionNames[i]:= 1]  
				  
				}
			
		  }
  
}

##http://gis.stackexchange.com/questions/63793/how-to-overlay-a-polygon-over-spatialpointsdataframe-and-preserving-the-spdf-dat

data.table(SalesMatrix)

## export file

myPath <- "C:/Garamvolgyi/Munka/Solar Lead/Output_1_20150716.xls"
	write.xlsx(
		SalesMatrix, 
		myPath, 
		sheetName = "SalesMatrix", 
		col.names = TRUE, 
		row.names = FALSE, 
		append=FALSE, 
		showNA = TRUE
		)

SalesMatrix <- SalesMatrix[, Sum:= BB + BFKR + 
					BJ + BL + Budapest + DMK + HJ + KI + PJ + RZS + SL + TTSZ ] 
					meltSalesMatrix <- melt(SalesMatrix, id=c("NEV", "HOSSZUSAG", "SZELESSEG", 'IRSZ'))
meltSalesMatrix <- meltSalesMatrix[!value==0]
meltSalesMatrix <- meltSalesMatrix[!variable=="Sum"]
setnames(meltSalesMatrix,"variable" , "Sales_region")
setnames(meltSalesMatrix,"value" , "Szerepel")

## http://www.r-statistics.com/2012/01/aggregation-and-restructuring-data-from-r-in-action/

myPath <- "C:/Garamvolgyi/Munka/Solar Lead/Output_2_20150716.xls"
	write.xlsx(
		meltSalesMatrix, 
		myPath, 
		sheetName = "SalesMatrix", 
		col.names = TRUE, 
		row.names = FALSE, 
		append=FALSE, 
		showNA = TRUE
		)

## graphics

myLocation <- "Hungary"

myMap <- get_map(
	location=myLocation, 
	source="google", 
	maptype="roadmap", 
	crop=FALSE, 
	zoom = 7) 

ggmap(myMap) + 
	geom_point(data=meltSalesMatrix, 
	           aes(x = HOSSZUSAG, y = SZELESSEG, colour=Sales_region, alpha=.2)) 
  + scale_colour_brewer(palette="Paired")

ggmap(myMap) + 
  geom_polygon(data=SalesRegionCoordinates, 
               aes(x=Hosszusag, y=Szelesseg, colour="red", fill=Sales_region,alpha=.03))

## http://docs.ggplot2.org/0.9.3.1/geom_point.html	
## http://docs.ggplot2.org/current/scale_brewer.html
## https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
# http://www.gadm.org/download
# http://stackoverflow.com/questions/17723822/administrative-regions-map-of-a-country-with-ggmap-and-ggplot2

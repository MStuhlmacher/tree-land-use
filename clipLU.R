#Michelle Stuhlmacher

#GOAL: Clip all the LU files to the urban extent and export smaller boundaries
#Needed to do this to lower the computational load of reclassifying land uses.
#Some land use files are for a much larger region than the city.

#STEPS:
#1) Bring in the LU and city boundary shapefiles
#2) Project to matching projection
#3) Clip cities that need to be clipped
#4) Export all cities with matching projection

# STEP 1 -----------------------------------------------
#Import data and libraries

#Libraries
library(raster)
library(dplyr)
#library(rgeos)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research") #work computer

#Import city boundaries
#Not all cities need to be clipped, these are not included here
ChicagoBounds = shapefile("./EJLSAMultiCity/Data/CityBoundaries/Chicago/CityBoundary.shp")
HoustonBounds = shapefile("./EJLSAMultiCity/Data/CityBoundaries/Houston/CityBoundary.shp")
PhoenixBounds = shapefile("./EJLSAMultiCity/Data/CityBoundaries/Phoenix/CityBoundary.shp")
PortlandBounds = shapefile("./EJLSAMultiCity/Data/CityBoundaries/Portland/CityBoundary.shp")
SeattleBounds = shapefile("./EJLSAMultiCity/Data/CityBoundaries/Seattle/CityBoundary.shp")

#Import city LU
#All land use files (even those that don't need to be clipped) need to be imported to reproject them to match the rest
ChicagoLU = shapefile("./TreeLocations/Data/ZoningLU/Chicago/Land_Use_Inventory_for_Northeastern_Illinois%2C_2018.shp")
HoustonLU = shapefile("./TreeLocations/Data/ZoningLU/Houston - SYNCED/Current_Future_Land_Use.shp")
IndianapolisLU = shapefile("./TreeLocations/Data/ZoningLU/Indianapolis/Current_Land_Use/Current_Land_Use.shp")
JacksonvilleLU = shapefile("./TreeLocations/Data/ZoningLU/Jacksonville - SYNCED/landuse/LUSE_20230822.shp")
LALU = shapefile("./TreeLocations/Data/ZoningLU/Los Angeles - SYNCED/AnnualLandUsePolySCAGv2.shp")
NewYorkLU = shapefile("./TreeLocations/Data/ZoningLU/NewYork - SYNCED/New York/nyc_mappluto_23v2_unclipped_shp/MapPLUTO_UNCLIPPED.shp")
PhoenixLU = shapefile("./TreeLocations/Data/ZoningLU/Phoenix - SYNCED/Existing_Land_Use_2022.shp")
PortlandLU = shapefile("./TreeLocations/Data/ZoningLU/Portland - SYNCED/Disaggregated_Zoning/Disaggregated_Zoning.shp")
SeattleLU = shapefile("./TreeLocations/Data/ZoningLU/Seattle - SYNCED/Current_Land_Use_Zoning_Detail/Zoning_Detailed.shp")
StLouisLU = shapefile("./TreeLocations/Data/ZoningLU/SaintLouis - SYNCED/St. Louis Zoning/zoning/prclz.shp")

# STEP 2 -----------------------------------------------
#Project to matching projection

#Proj4 string for Albers Equal Area Conic projection (good for contiguous US)
#https://rdrr.io/github/JGCRI/gcammaptools/man/na_aea.html
projAEC = '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83'

##Chicago
crs(ChicagoBounds)
crs(ChicagoLU)

ChicagoBoundsRP = spTransform(ChicagoBounds, crs(projAEC))
crs(ChicagoBoundsRP)
ChicagoLURP = spTransform(ChicagoLU, crs(projAEC))
crs(ChicagoLURP)

##Houston
crs(HoustonBounds)
crs(HoustonLU)

HoustonBoundsRP = spTransform(HoustonBounds, crs(projAEC))
crs(HoustonBoundsRP)
HoustonLURP = spTransform(HoustonLU, crs(projAEC))
crs(HoustonLURP)

##Indianapolis (only need to reproject LU)
crs(IndianapolisLU)
IndianapolisLURP = spTransform(IndianapolisLU, crs(projAEC))
crs(IndianapolisLURP)

##Jacksonville (only need to reproject LU)
crs(JacksonvilleLU)
JacksonvilleLURP = spTransform(JacksonvilleLU, crs(projAEC))
crs(JacksonvilleLURP)

##Los Angeles (only need to reproject LU)
LALURP = spTransform(LALU, crs(projAEC))
crs(LALURP)

##New York (only need to reproject LU)
NewYorkLURP = spTransform(NewYorkLU, crs(projAEC))

##Phoenix
PhoenixBoundsRP = spTransform(PhoenixBounds, crs(projAEC))
crs(PhoenixBoundsRP)
PhoenixLURP = spTransform(PhoenixLU, crs(projAEC))
crs(PhoenixLURP)

##Portland
PortlandBoundsRP = spTransform(PortlandBounds, crs(projAEC))
crs(PortlandBoundsRP)
PortlandLURP = spTransform(PortlandLU, crs(projAEC))
crs(PortlandLURP)

##Seattle
SeattleBoundsRP = spTransform(SeattleBounds, crs(projAEC))
crs(SeattleBoundsRP)
SeattleLURP = spTransform(SeattleLU, crs(projAEC))
crs(SeattleLURP)

##Saint Louis (only need to reproject LU)
StLouisLURP = spTransform(StLouisLU, crs(projAEC))
crs(StLouisLURP)

# STEP 3 -----------------------------------------------
#Clip (if necessary)

##Chicago
LUchi = intersect(ChicagoLURP,ChicagoBoundsRP)

#Remove all the features associated with Chicago bounds: "shape_area" etc.
LUchi = LUchi[,!(names(LUchi) %in% c("shape_area","shape_len","objectid"))] 

##Houston
#add a column to the Houston bounds file
HoustonBoundsRP$name = "Houston"
#Remove "SHAPE_Area" column because it causes errors
HoustonLURP = HoustonLURP[,!(names(HoustonLURP) %in% c("SHAPE_Area","Acres","SHAPE_Leng"))]
#run intersection
LUhou = raster::intersect(HoustonLURP,HoustonBoundsRP)

##Los Angeles
#Remove long columns
LALURP = LALURP[,!(names(LALURP) %in% c("SHAPE_Area"))]

##New York
#Remove features with a large number of digits because they don't write out successfully
#BBL, APPBL, AssessTot, ExemptTot, Shape_Area
NewYorkLURP = NewYorkLURP[,!(names(NewYorkLURP) %in% c("BBL","APPBL","AssessTot","ExemptTot","Shape_Area"))]

##Phoenix
#Do a zero-width buffer to see if it fixes this error:
#Error in createPolygonsComment(p): rgeos_PolyCreateComment: orphaned hole, cannot find containing polygon for hole at index 2
PhoenixBoundsRP = gBuffer(PhoenixBoundsRP, byid = T, width = 0)
PhoenixLURP = gBuffer(PhoenixLURP, byid = T, width = 0)
LUphx = raster::intersect(PhoenixLURP,PhoenixBoundsRP)

##Portland
LUpor = raster::intersect(PortlandLURP,PortlandBoundsRP)
LUpor = LUpor[,!(names(LUpor) %in% c("Shape_Area.1","Shape_Leng.2","Shape_Area.2","AREA"))]

##Seattle
LUsea = raster::intersect(SeattleLURP,SeattleBoundsRP)
LUsea = LUsea[,!(names(LUsea) %in% c("SHAPE_Area.1","SHAPE_Leng.1","SHAPE_Leng.2","SHAPE_Area.2"))]

# STEP 5 -----------------------------------------------
#Export

##Chicago
shapefile(LUchi,'./TreeLocations/Data/ZoningLUClipped/Chicago/ChicagoLU.shp')

##Houston
shapefile(LUhou,'./TreeLocations/Data/ZoningLUClipped/Houston/HoustonLU.shp')

##Indianapolis
shapefile(IndianapolisLURP,'./TreeLocations/Data/ZoningLUClipped/Indianapolis/IndianapolisLU.shp')

##Jacksonville
shapefile(JacksonvilleLURP,'./TreeLocations/Data/ZoningLUClipped/Jacksonville/JacksonvilleLU.shp')

##Los Angeles
shapefile(LALURP,'./TreeLocations/Data/ZoningLUClipped/LosAngeles/LosAngelesLU.shp')

##New York
shapefile(NewYorkLURP,'./TreeLocations/Data/ZoningLUClipped/NewYork/NewYorkLU.shp')

##Phoenix
shapefile(LUphx,'./TreeLocations/Data/ZoningLUClipped/Phoenix/PhoenixLU.shp')

##Portland
shapefile(LUpor,'./TreeLocations/Data/ZoningLUClipped/Portland/PortlandLU.shp')

##Seattle
shapefile(LUsea,'./TreeLocations/Data/ZoningLUClipped/Seattle/SeattleLU.shp')

##Saint Louis
shapefile(StLouisLURP,'./TreeLocations/Data/ZoningLUClipped/SaintLouis/SaintLouisLU.shp')

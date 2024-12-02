#Michelle Stuhlmacher

#GOAL: 1) Calculate the percent tree canopy cover by land use & 2) Calculate total tree canopy cover

#STEPS:
#1) Import data and packages
#2) Run percent tree cover in loop by LU type
#3) Format output and export
#4) Calculate total tree canopy coverage for Table 3

# STEP 1 -----------------------------------------------
#set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/TreeLocations")

#Import libraries
library(raster)
library(landscapemetrics)
library(sf)
library(dplyr)

#Import 5m tree rasters
treeCHI = raster("D:/GoogleTreeCanopy/Chicago_5m/GoogleTreeCanopy_Chicago_5mAlbers.tif")
treeHOU = raster("D:/GoogleTreeCanopy/Houston_5m/GoogleTreeCanopy_Houston_5mAlbers.tif")
treeIND = raster("D:/GoogleTreeCanopy/Indianapolis_5m/GoogleTreeCanopy_Indianapolis_5mAlbers.tif")
treeJAC = raster("D:/GoogleTreeCanopy/Jacksonville_5m/GoogleTreeCanopy_Jacksonville_5mAlbers.tif")
treeLAX = raster("D:/GoogleTreeCanopy/LA_5m/GoogleTreeCanopy_LosAngeles_5mAlbers.tif")
treeNYC = raster("D:/GoogleTreeCanopy/NewYork_5m/GoogleTreeCanopy_NewYork_5mAlbers.tif")
treePHX = raster("D:/GoogleTreeCanopy/Phoenix_5m/GoogleTreeCanopy_Phoenix_5mAlbers.tif")
treePOR = raster("D:/GoogleTreeCanopy/Portland_5m/GoogleTreeCanopy_Portland_5mAlbers.tif")
treeSTL = raster("D:/GoogleTreeCanopy/StLouis_5m/GoogleTreeCanopy_StLouis_5mAlbers.tif")
treeSEA = raster("D:/GoogleTreeCanopy/Seattle_5m/GoogleTreeCanopy_Seattle_5mAlbers.tif")

#Import reclassed zoning shapefiles
zonCHI = st_read('./Data/ZoningLUClipped/Chicago/ChicagoLUTPL_5classes.shp')
zonHOU = st_read('./Data/ZoningLUClipped/Houston/HoustonLUTPL_5classes.shp')
zonIND = st_read('./Data/ZoningLUClipped/Indianapolis/IndianapolisLU_5classes.shp')
zonJAC = st_read('./Data/ZoningLUClipped/Jacksonville/JacksonvilleLUTPL_5classes.shp')
zonLAX = st_read('./Data/ZoningLUClipped/LosAngeles/LosAngelesLU_5classes.shp')
zonNYC = st_read('./Data/ZoningLUClipped/NewYork/NewYorkLU_5classes.shp')
zonPHX = st_read('./Data/ZoningLUClipped/Phoenix/PhoenixLUTPL_5classes.shp')
zonPOR = st_read('./Data/ZoningLUClipped/Portland/PortlandLUTPL_5classes.shp')
zonSTL = st_read('./Data/ZoningLUClipped/SaintLouis/SaintLouisLUTPL_5classes.shp')
zonSEA = st_read('./Data/ZoningLUClipped/Seattle/SeattleLUTPL_5classes.shp')

#Proj4 string for Albers Equal Area Conic projection (good for contiguous US)
#https://rdrr.io/github/JGCRI/gcammaptools/man/na_aea.html
projAEC = '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83'

# STEP 2 -----------------------------------------------
#Loop through land cover and calculate pland and area
tree = treeHOU #set the city equal to zon variable in loop
zon = st_transform(zonHOU, crs(treeHOU))

DF_TREE = data.frame(matrix(ncol = 6, nrow = 0))
colnames(DF_TREE) = c('layer','level','class','id','metric','value')

#plot(zon, col = "blue", max.plot = 1)

for(i in 1:nrow(zon)) {
  print("loop step 0")
  parcel = zon[i,]
  print("loop step 1")
  crop = crop(tree,parcel)
  print("loop step 2")
  mask = mask(crop,parcel)
  print("loop step 3")
  lsm = calculate_lsm(mask, what = c('lsm_c_pland','lsm_c_ca'),verbose=T)
  print("loop step 4")
  lsm$id = parcel$sLANDUSE
  print("loop step 5")
  DF_TREE = rbind(DF_TREE,lsm)
  print(cat('LU Type #',parcel$sLANDUSE))
  #gc()
}

# STEP 3 -----------------------------------------------
#Format output and export

#Remove non-tree class
DF_TREE = DF_TREE[DF_TREE$class == '1', ]

#Remove extra columns
DF_TREE = DF_TREE[ , c("id","metric","value")]

#Reformat from long to wide
DF_TREEw = reshape(data = data.frame(DF_TREE),
                   v.names = c("value"),
                   idvar= "id",
                   timevar= "metric",
                   direction = "wide")

#Clean up and rename columns
DF_TREEw = DF_TREEw[ , c("id","value.ca","value.pland")]
colnames(DF_TREEw) = c("id","ca","pland")

#Export
write.csv(DF_TREEw,'./Results/TreeZoneArea/HoustonTZA_20240217.csv')

# STEP 4 -----------------------------------------------
#Import city boundary shapefiles
boundCHI = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/CityBoundaries/Chicago/CityBoundary.shp")
boundHOU = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/CityBoundaries/Houston/CityBoundary.shp")
boundIND = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/CityBoundaries/Indianapolis/CityBoundary.shp")
boundJAC = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/CityBoundaries/Jacksonville/CityBoundary.shp")
boundLAX = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/CityBoundaries/LA/CityBoundary.shp")
boundNYC = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/CityBoundaries/NewYork/CityBoundary.shp")
boundPHX = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/CityBoundaries/Phoenix/CityBoundary.shp")
boundPOR = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/CityBoundaries/Portland/CityBoundary.shp")
boundSTL = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/CityBoundaries/StLouis/CityBoundary.shp")
boundSEA = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/CityBoundaries/Seattle/CityBoundary.shp")

#Reproject boundary to match tree raster
boundsCHI = st_transform(boundCHI, crs(treeCHI))
boundsHOU = st_transform(boundHOU, crs(treeHOU))
boundsIND = st_transform(boundIND, crs(treeIND))
boundsJAC = st_transform(boundJAC, crs(treeJAC))
boundsLAX = st_transform(boundLAX, crs(treeLAX))
boundsNYC = st_transform(boundNYC, crs(treeNYC))
boundsPHX = st_transform(boundPHX, crs(treePHX))
boundsPOR = st_transform(boundPOR, crs(treePOR))
boundsSTL = st_transform(boundSTL, crs(treeSTL))
boundsSEA = st_transform(boundSEA, crs(treeSEA))

##CHICAGO##
cropCHI = crop(treeCHI,boundsCHI)
maskCHI = mask(cropCHI,boundsCHI)
#plot(maskCHI)
treeonlyCHI = mask(maskCHI,maskCHI,maskvalue = 0,updatevalue = NA)
plot(treeonlyCHI)

#cellStats(mask, 'sum') #4387420
#cellStats(treeonly, 'sum')
treePixCHI = sum(values(treeonlyCHI), na.rm=TRUE)
treeACHI = (treePixCHI * 25)/1000000
#Chicago = 4387420 pixels. Each pixel is 5m = 4387420 x 25m2 = 109,685,500m2 = 109.6855 km2 = 42 square miles
AreaCHI = st_area(boundsCHI)
#Chicago = 599239360m2 = 599.24 km2 = 231.37 square miles
treePctCHI = treeACHI/(AreaCHI/1000000)
#Chicago = 18.3% tree canopy coverage

##HOUSTON##
cropHOU = crop(treeHOU,boundsHOU)
maskHOU = mask(cropHOU,boundsHOU)
treeonlyHOU = mask(maskHOU,maskHOU,maskvalue = 0,updatevalue = NA)
plot(treeonlyHOU)

treePixHOU = sum(values(treeonlyHOU), na.rm=TRUE)
treeAHOU = (treePixHOU * 25)/1000000
AreaHOU = st_area(boundsHOU)
treePctHOU = treeAHOU/(AreaHOU/1000000)

##INDIANAPOLIS##
cropIND = crop(treeIND,boundsIND)
maskIND = mask(cropIND,boundsIND)
treeonlyIND = mask(maskIND,maskIND,maskvalue = 0,updatevalue = NA)
plot(treeonlyIND)

treePixIND = sum(values(treeonlyIND), na.rm=TRUE)
treeAIND = (treePixIND * 25)/1000000
AreaIND = st_area(boundsIND)
treePctIND = treeAIND/(AreaIND/1000000)

##JACKSONVILLE##
cropJAC = crop(treeJAC,boundsJAC)
maskJAC = mask(cropJAC,boundsJAC)
treeonlyJAC = mask(maskJAC,maskJAC,maskvalue = 0,updatevalue = NA)
plot(treeonlyJAC)

treePixJAC = sum(values(treeonlyJAC), na.rm=TRUE)
treeAJAC = (treePixJAC * 25)/1000000
AreaJAC = st_area(boundsJAC)
treePctJAC = treeAJAC/(AreaJAC/1000000)

##LOS ANGELES##
cropLAX = crop(treeLAX,boundsLAX)
maskLAX = mask(cropLAX,boundsLAX)
treeonlyLAX = mask(maskLAX,maskLAX,maskvalue = 0,updatevalue = NA)
plot(treeonlyLAX)

treePixLAX = sum(values(treeonlyLAX), na.rm=TRUE)
treeALAX = (treePixLAX * 25)/1000000
AreaLAX = st_area(boundsLAX)
treePctLAX = treeALAX/(AreaLAX/1000000)

##NEW YORK##
cropNYC = crop(treeNYC,boundsNYC)
maskNYC = mask(cropNYC,boundsNYC)
treeonlyNYC = mask(maskNYC,maskNYC,maskvalue = 0,updatevalue = NA)
plot(treeonlyNYC)

treePixNYC = sum(values(treeonlyNYC), na.rm=TRUE)
treeANYC = (treePixNYC * 25)/1000000
AreaNYC = st_area(boundsNYC)
treePctNYC = treeANYC/(AreaNYC/1000000)

##PHOENIX##
cropPHX = crop(treePHX,boundsPHX)
maskPHX = mask(cropPHX,boundsPHX)
treeonlyPHX = mask(maskPHX,maskPHX,maskvalue = 0,updatevalue = NA)
plot(treeonlyPHX)

treePixPHX = sum(values(treeonlyPHX), na.rm=TRUE)
treeAPHX = (treePixPHX * 25)/1000000
AreaPHX = st_area(boundsPHX)
treePctPHX = treeAPHX/(AreaPHX/1000000)

##PORTLAND##
cropPOR = crop(treePOR,boundsPOR)
maskPOR = mask(cropPOR,boundsPOR)
treeonlyPOR = mask(maskPOR,maskPOR,maskvalue = 0,updatevalue = NA)
plot(treeonlyPOR)

treePixPOR = sum(values(treeonlyPOR), na.rm=TRUE)
treeAPOR = (treePixPOR * 25)/1000000
AreaPOR = st_area(boundsPOR)
treePctPOR = treeAPOR/(AreaPOR/1000000)

##SAINT LOUIS##
cropSTL = crop(treeSTL,boundsSTL)
maskSTL = mask(cropSTL,boundsSTL)
treeonlySTL = mask(maskSTL,maskSTL,maskvalue = 0,updatevalue = NA)
plot(treeonlySTL)

treePixSTL = sum(values(treeonlySTL), na.rm=TRUE)
treeASTL = (treePixSTL * 25)/1000000
AreaSTL = st_area(boundsSTL)
treePctSTL = treeASTL/(AreaSTL/1000000)

##SEATTLE##
cropSEA = crop(treeSEA,boundsSEA)
maskSEA = mask(cropSEA,boundsSEA)
treeonlySEA = mask(maskSEA,maskSEA,maskvalue = 0,updatevalue = NA)
plot(treeonlySEA)

treePixSEA = sum(values(treeonlySEA), na.rm=TRUE)
treeASEA = (treePixSEA * 25)/1000000
AreaSEA = st_area(boundsSEA)
treePctSEA = treeASEA/(AreaSEA/1000000)


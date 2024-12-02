#Michelle Stuhlmacher

##GOALS:
#Recode the LU and zoning shapefiles for each city to have the same classes
#Class breakdown table is in the appendix of article

#STEPS:
#1) Import data and libraries
#2) Clean TPL data and join with LU/zoning to prevent double counts
#3) Reclassify LU/zoning in each city
#4) Calculate proportion of land use belonging to each class
#5) Export reclassified shapefile
#6) Count the number of classes in original shp for Table 1

# STEP 1 -----------------------------------------------
# Import data and libraries
library(sf)
library(terra)
library(ggplot2)
library(dplyr)

#set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/TreeLocations")

#Import LU and zoning shapefiles
zonCHI = st_read("./Data/ZoningLUClipped/Chicago/ChicagoLU.shp")
zonHOU = st_read("./Data/ZoningLUClipped/Houston/HoustonLU.shp")
zonIND = st_read("./Data/ZoningLUClipped/Indianapolis/IndianapolisLU.shp")
zonJAC = st_read("./Data/ZoningLUClipped/Jacksonville/JacksonvilleLU.shp")
zonLAX = st_read("./Data/ZoningLUClipped/LosAngeles/LosAngelesLU.shp")
zonNYC = st_read("./Data/ZoningLUClipped/NewYork/NewYorkLU.shp")
zonPHX = st_read("./Data/ZoningLUClipped/Phoenix/PhoenixLU.shp")
zonPOR = st_read("./Data/ZoningLUClipped/Portland/PortlandLU.shp")
zonSTL = st_read("./Data/ZoningLUClipped/SaintLouis/SaintLouisLU.shp")
zonSEA = st_read("./Data/ZoningLUClipped/Seattle/SeattleLU.shp")

#Import TPL park shapefile
parkCHI = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/Park/CityBoxClip/ParkServe_Parks_05182021_ChicagoBox.shp")
parkHOU = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/Park/CityBoxClip/ParkServe_Parks_05182021_HoustonBox.shp")
parkIND = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/Park/CityBoxClip/ParkServe_Parks_05182021_IndianapolisBox.shp")
parkJAC = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/Park/CityBoxClip/ParkServe_Parks_05182021_JacksonvilleBox.shp")
parkLAX = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/Park/CityBoxClip/ParkServe_Parks_05182021_LABox.shp")
parkNYC = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/Park/CityBoxClip/ParkServe_Parks_05182021_NewYorkBox.shp")
parkPHX = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/Park/CityBoxClip/ParkServe_Parks_05182021_PhoenixBox.shp")
parkPOR = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/Park/CityBoxClip/ParkServe_Parks_05182021_PortlandBox.shp")
parkSTL = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/Park/CityBoxClip/ParkServe_Parks_05182021_StLouisBox.shp")
parkSEA = st_read("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity/Data/Park/CityBoxClip/ParkServe_Parks_05182021_SeattleBox.shp")

#st_is_valid(parkHOU)

#Import city boundaries
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

# STEP 2 -----------------------------------------------
#Clean TPL data and join with LU/zoning to prevent double counts

# ----Chicago----
boundsCHI = st_transform(boundCHI,crs(parkCHI)) #Reproject city boundaries to match TPL
parkCHI = st_intersection(parkCHI,boundsCHI) #Clip TPL to city boundaries

#Remove TPL extent from LU/zoning vector
CHIint = st_intersection(parkCHI, zonCHI) #intersection
CHIdiff = st_difference(zonCHI, st_union(st_geometry(CHIint))) #difference to show only zoning that does not intersect with TPL

# ----Houston----
boundsHOU = st_transform(boundHOU,crs(parkHOU)) #Reproject city boundaries to match TPL
parkHOU = st_intersection(parkHOU,boundsHOU) #Clip TPL to city boundaries

#Remove TPL extent from LU/zoning vector
HOUint = st_intersection(parkHOU, zonHOU) #intersection
HOUdiff = st_difference(zonHOU, st_union(st_geometry(HOUint))) #difference to show only zoning that does not intersect with TPL
#plot(HOUdiff, col = "blue", max.plot = 1)

# ----Indianapolis----
boundsIND = st_transform(boundIND,crs(parkIND)) #Reproject city boundaries to match TPL
parkIND = st_intersection(parkIND,boundsIND) #Clip TPL to city boundaries

#Remove TPL extent from LU/zoning vector
INDint = st_intersection(parkIND, zonIND) #intersection
INDdiff = st_difference(zonIND, st_union(st_geometry(INDint))) #difference to show only zoning that does not intersect with TPL

# ----Jacksonville----
boundsJAC = st_transform(boundJAC,crs(parkJAC)) #Reproject city boundaries to match TPL
parkJAC = st_intersection(parkJAC,boundsJAC) #Clip TPL to city boundaries

#Remove TPL extent from LU/zoning vector
JACint = st_intersection(parkJAC, zonJAC) #intersection
JACdiff = st_difference(zonJAC, st_union(st_geometry(JACint))) #difference to show only zoning that does not intersect with TPL

# ----Los Angeles----
boundsLAX = st_transform(boundLAX,crs(parkLAX)) #Reproject city boundaries to match TPL
parkLAX = st_intersection(parkLAX,boundsLAX) #Clip TPL to city boundaries

#Remove TPL extent from LU/zoning vector
LAXint = st_intersection(parkLAX, zonLAX) #intersection
LAXdiff = st_difference(zonLAX, st_union(st_geometry(LAXint))) #difference to show only zoning that does not intersect with TPL

# ----New York----
boundsNYC = st_transform(boundNYC,crs(parkNYC)) #Reproject city boundaries to match TPL
parkNYC = st_intersection(parkNYC,boundsNYC) #Clip TPL to city boundaries

#Remove TPL extent from LU/zoning vector
NYCint = st_intersection(parkNYC, zonNYC) #intersection
NYCdiff = st_difference(zonNYC, st_union(st_geometry(NYCint))) #difference to show only zoning that does not intersect with TPL

# ----Phoenix----
boundsPHX = st_transform(boundPHX,crs(parkPHX)) #Reproject city boundaries to match TPL
parkPHX = st_intersection(parkPHX,boundsPHX) #Clip TPL to city boundaries

#Remove TPL extent from LU/zoning vector
PHXint = st_intersection(parkPHX, zonPHX) #intersection
PHXdiff = st_difference(zonPHX, st_union(st_geometry(PHXint))) #difference to show only zoning that does not intersect with TPL

# ----Portland----
boundsPOR = st_transform(boundPOR,crs(parkPOR)) #Reproject city boundaries to match TPL
parkPOR = st_intersection(parkPOR,boundsPOR) #Clip TPL to city boundaries

#First, select only "zon_typ" == "base" (not overlay)
zonPOR = zonPOR[zonPOR$zon_typ == 'base',]

#Remove TPL extent from LU/zoning vector
PORint = st_intersection(parkPOR, zonPOR) #intersection
PORdiff = st_difference(zonPOR, st_union(st_geometry(PORint))) #difference to show only zoning that does not intersect with TPL

# ----Saint Louis----
#Reproject city boundaries to match TPL
crs(parkSTL)
crs(boundSTL)
boundsSTL = st_transform(boundSTL,crs(parkSTL))
#st_is_valid(boundsSTL)

#Clip TPL to city boundaries
parkSTL = st_intersection(parkSTL,boundsSTL)
#plot(parkSTL, col = "#4daf4a", max.plot = 1)

#Reproject TPL to match zoning (if needed)
crs(zonSTL)

#Remove TPL extent from LU/zoning vector
STLint = st_intersection(parkSTL, zonSTL) #intersection
#plot(STLint, col = "red", max.plot = 1)
STLdiff = st_difference(zonSTL, st_union(st_geometry(STLint))) #difference to show only zoning that does not intersect with TPL
plot(STLdiff, col = "blue", max.plot = 1)

# ----Seattle----
boundsSEA = st_transform(boundSEA,crs(parkSEA)) #Reproject city boundaries to match TPL
parkSEA = st_intersection(parkSEA,boundsSEA) #Clip TPL to city boundaries

#Remove TPL extent from LU/zoning vector
SEAint = st_intersection(parkSEA, zonSEA) #intersection
SEAdiff = st_difference(zonSEA, st_union(st_geometry(SEAint))) #difference to show only zoning that does not intersect with TPL

# STEP 3 -----------------------------------------------
# Reclassify each city

#Simplified classes:
#1. Single-family residential
#2. Multi-family residential
#3. Parks* 
#4. Industrial 
#5. Other 

#----Chicago----
CHIdiff$sLANDUSE = ifelse(CHIdiff$LANDUSE == 1110 | CHIdiff$LANDUSE == 1111 | CHIdiff$LANDUSE == 1112 | CHIdiff$LANDUSE == 1140, 1, #single-family residential
                         ifelse(CHIdiff$LANDUSE == 1130, 2, #multi-family residential
                                ifelse(CHIdiff$LANDUSE == 1410 | CHIdiff$LANDUSE == 1420 | CHIdiff$LANDUSE == 1430 | CHIdiff$LANDUSE == 1431 | 
                                         CHIdiff$LANDUSE == 1432 | CHIdiff$LANDUSE == 1433 | CHIdiff$LANDUSE == 1450, 4, #industrial
                                              5))) #all other classes

#Simplify LU by merging polygons with matching codes
zonCHIf = CHIdiff %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#Add new column to parks to match reclassified vector (clean up other columns)
parkCHI$sLANDUSE = 3
parkCHI = parkCHI[,(names(parkCHI) %in% c("sLANDUSE","ParkID"))] #Keep only sLANDUSE, ParkID, and geometry

#Remove unused vectors for for memory conservation
rm(zonCHI)
rm(CHIint)
rm(boundCHI)
rm(CHIdiff)
gc()

#Add the two vectors together
zonCHIfin = bind_rows(zonCHIf,parkCHI)
#st_is_valid(zonCHIfin)

#simplify parks
zonCHIfin = zonCHIfin %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#Check that it worked as expected by plotting
palette = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00")
colors = palette[as.numeric(zonCHIfin$sLANDUSE)]
plot(zonCHIfin, col = colors, max.plot = 1)

# ----Houston----
#If else statement, using 51% or greater as cut off
#See step 4 below for generating unique list of LU and percentages for building if else statement

HOUdiff$sLANDUSE = ifelse(grepl("Single-Family 51", HOUdiff$Current_La) | grepl("Single-Family 52", HOUdiff$Current_La) | grepl("Single-Family 53", HOUdiff$Current_La) | 
                            grepl("Single-Family 54", HOUdiff$Current_La) | grepl("Single-Family 55", HOUdiff$Current_La) | grepl("Single-Family 56", HOUdiff$Current_La) | 
                            grepl("Single-Family 57", HOUdiff$Current_La) | grepl("Single-Family 58", HOUdiff$Current_La) | grepl("Single-Family 59", HOUdiff$Current_La) | 
                            grepl("Single-Family 60", HOUdiff$Current_La) | grepl("Single-Family 61", HOUdiff$Current_La) | grepl("Single-Family 62", HOUdiff$Current_La) | 
                            grepl("Single-Family 63", HOUdiff$Current_La) | grepl("Single-Family 64", HOUdiff$Current_La) | grepl("Single-Family 65", HOUdiff$Current_La) | 
                            grepl("Single-Family 66", HOUdiff$Current_La) | grepl("Single-Family 67", HOUdiff$Current_La) | grepl("Single-Family 68", HOUdiff$Current_La) | 
                            grepl("Single-Family 69", HOUdiff$Current_La) | grepl("Single-Family 70", HOUdiff$Current_La) | grepl("Single-Family 71", HOUdiff$Current_La) | 
                            grepl("Single-Family 72", HOUdiff$Current_La) | grepl("Single-Family 73", HOUdiff$Current_La) | grepl("Single-Family 74", HOUdiff$Current_La) | 
                            grepl("Single-Family 75", HOUdiff$Current_La) | grepl("Single-Family 76", HOUdiff$Current_La) | grepl("Single-Family 77", HOUdiff$Current_La) | 
                            grepl("Single-Family 78", HOUdiff$Current_La) | grepl("Single-Family 79", HOUdiff$Current_La) | grepl("Single-Family 80", HOUdiff$Current_La) | 
                            grepl("Single-Family 81", HOUdiff$Current_La) | grepl("Single-Family 82", HOUdiff$Current_La) | grepl("Single-Family 83", HOUdiff$Current_La) | 
                            grepl("Single-Family 84", HOUdiff$Current_La) | grepl("Single-Family 85", HOUdiff$Current_La) | grepl("Single-Family 86", HOUdiff$Current_La) | 
                            grepl("Single-Family 87", HOUdiff$Current_La) | grepl("Single-Family 88", HOUdiff$Current_La) | grepl("Single-Family 89", HOUdiff$Current_La) | 
                            grepl("Single-Family 90", HOUdiff$Current_La) | grepl("Single-Family 100", HOUdiff$Current_La) |
                            
                            grepl("Mobile Homes 51", HOUdiff$Current_La) | grepl("Mobile Homes 52", HOUdiff$Current_La) | grepl("Mobile Homes 53", HOUdiff$Current_La) | 
                            grepl("Mobile Homes 54", HOUdiff$Current_La) | grepl("Mobile Homes 55", HOUdiff$Current_La) | grepl("Mobile Homes 56", HOUdiff$Current_La) | 
                            grepl("Mobile Homes 57", HOUdiff$Current_La) | grepl("Mobile Homes 58", HOUdiff$Current_La) | grepl("Mobile Homes 59", HOUdiff$Current_La) | 
                            grepl("Mobile Homes 60", HOUdiff$Current_La) | grepl("Mobile Homes 61", HOUdiff$Current_La) | grepl("Mobile Homes 62", HOUdiff$Current_La) | 
                            grepl("Mobile Homes 63", HOUdiff$Current_La) | grepl("Mobile Homes 64", HOUdiff$Current_La) | grepl("Mobile Homes 65", HOUdiff$Current_La) | 
                            grepl("Mobile Homes 67", HOUdiff$Current_La) | grepl("Mobile Homes 68", HOUdiff$Current_La) | grepl("Mobile Homes 69", HOUdiff$Current_La) | 
                            grepl("Mobile Homes 70", HOUdiff$Current_La) | grepl("Mobile Homes 71", HOUdiff$Current_La) | grepl("Mobile Homes 72", HOUdiff$Current_La) | 
                            grepl("Mobile Homes 73", HOUdiff$Current_La) | grepl("Mobile Homes 75", HOUdiff$Current_La) | grepl("Mobile Homes 76", HOUdiff$Current_La) | 
                            grepl("Mobile Homes 77", HOUdiff$Current_La) | grepl("Mobile Homes 79", HOUdiff$Current_La) | grepl("Mobile Homes 89", HOUdiff$Current_La) | 
                            grepl("Mobile Homes 90", HOUdiff$Current_La) | grepl("Mobile Home 100", HOUdiff$Current_La) | grepl("Mobile Homes 100", HOUdiff$Current_La), 1, #single family residential
                          
                          ifelse(grepl("Condo 51", HOUdiff$Current_La) | grepl("Condo 52", HOUdiff$Current_La) | grepl("Condo 53", HOUdiff$Current_La) | grepl("Condo 54", HOUdiff$Current_La) | 
                                   grepl("Condo 55", HOUdiff$Current_La) | grepl("Condo 56", HOUdiff$Current_La) | grepl("Condo 57", HOUdiff$Current_La) | grepl("Condo 58", HOUdiff$Current_La) | 
                                   grepl("Condo 59", HOUdiff$Current_La) | grepl("Condo 60", HOUdiff$Current_La) | grepl("Condo 61", HOUdiff$Current_La) | grepl("Condo 62", HOUdiff$Current_La) | 
                                   grepl("Condo 63", HOUdiff$Current_La) | grepl("Condo 64", HOUdiff$Current_La) | grepl("Condo 65", HOUdiff$Current_La) | grepl("Condo 66", HOUdiff$Current_La) | 
                                   grepl("Condo 67", HOUdiff$Current_La) | grepl("Condo 68", HOUdiff$Current_La) | grepl("Condo 69", HOUdiff$Current_La) | grepl("Condo 70", HOUdiff$Current_La) | 
                                   grepl("Condo 71", HOUdiff$Current_La) | grepl("Condo 72", HOUdiff$Current_La) | grepl("Condo 73", HOUdiff$Current_La) | grepl("Condo 74", HOUdiff$Current_La) | 
                                   grepl("Condo 75", HOUdiff$Current_La) | grepl("Condo 76", HOUdiff$Current_La) | grepl("Condo 77", HOUdiff$Current_La) | grepl("Condo 78", HOUdiff$Current_La) |
                                   grepl("Condo 79", HOUdiff$Current_La) | grepl("Condo 80", HOUdiff$Current_La) | grepl("Condo 81", HOUdiff$Current_La) | grepl("Condo 82", HOUdiff$Current_La) | 
                                   grepl("Condo 83", HOUdiff$Current_La) | grepl("Condo 84", HOUdiff$Current_La) | grepl("Condo 85", HOUdiff$Current_La) | grepl("Condo 86", HOUdiff$Current_La) | 
                                   grepl("Condo 87", HOUdiff$Current_La) | grepl("Condo 88", HOUdiff$Current_La) | grepl("Condo 89", HOUdiff$Current_La) | grepl("Condo 90", HOUdiff$Current_La) | 
                                   grepl("Condo 100", HOUdiff$Current_La) | 
                                   
                                   grepl("Multi-Family 51", HOUdiff$Current_La) | grepl("Multi-Family 52", HOUdiff$Current_La) | grepl("Multi-Family 53", HOUdiff$Current_La) |
                                   grepl("Multi-Family 57", HOUdiff$Current_La) | grepl("Multi-Family 58", HOUdiff$Current_La) | grepl("Multi-Family 59", HOUdiff$Current_La) | 
                                   grepl("Multi-Family 62", HOUdiff$Current_La) | grepl("Multi-Family 64", HOUdiff$Current_La) | grepl("Multi-Family 65", HOUdiff$Current_La) | 
                                   grepl("Multi-Family 67", HOUdiff$Current_La) | grepl("Multi-Family 68", HOUdiff$Current_La) | grepl("Multi-Family 69", HOUdiff$Current_La) |
                                   grepl("Multi-Family 70", HOUdiff$Current_La) | grepl("Multi-Family 71", HOUdiff$Current_La) | grepl("Multi-Family 72", HOUdiff$Current_La) | 
                                   grepl("Multi-Family 73", HOUdiff$Current_La) | grepl("Multi-Family 77", HOUdiff$Current_La) | grepl("Multi-Family 78", HOUdiff$Current_La) | 
                                   grepl("Multi-Family 80", HOUdiff$Current_La) | grepl("Multi-Family 82", HOUdiff$Current_La) | grepl("Multi-Family 83", HOUdiff$Current_La) | 
                                   grepl("Multi-Family 87", HOUdiff$Current_La) | grepl("Multi-Family 88", HOUdiff$Current_La) | grepl("Multi-Family 90", HOUdiff$Current_La) | 
                                   grepl("Multi-Family 100", HOUdiff$Current_La) |
                                   
                                   grepl("Group Quarters 51", HOUdiff$Current_La) | grepl("Group Quarters 53", HOUdiff$Current_La) | grepl("Group Quarters 62", HOUdiff$Current_La) |
                                   grepl("Group Quarters 68", HOUdiff$Current_La) | grepl("Group Quarters 69", HOUdiff$Current_La) | grepl("Group Quarters 71", HOUdiff$Current_La) | 
                                   grepl("Group Quarters 73", HOUdiff$Current_La) | grepl("Group Quarters 87", HOUdiff$Current_La) | grepl("Group Quarters 89", HOUdiff$Current_La) |
                                   grepl("Group Quarters 100", HOUdiff$Current_La), 2, #multi-family residential
                                
                                  ifelse(grepl("Industrial 51", HOUdiff$Current_La) | grepl("Industrial 52", HOUdiff$Current_La) | grepl("Industrial 53", HOUdiff$Current_La) | 
                                           grepl("Industrial 54", HOUdiff$Current_La) | grepl("Industrial 55", HOUdiff$Current_La) | grepl("Industrial 56", HOUdiff$Current_La) | 
                                           grepl("Industrial 58", HOUdiff$Current_La) | grepl("Industrial 59", HOUdiff$Current_La) | grepl("Industrial 60", HOUdiff$Current_La) | 
                                           grepl("Industrial 61", HOUdiff$Current_La) | grepl("Industrial 62", HOUdiff$Current_La) | grepl("Industrial 63", HOUdiff$Current_La) |
                                           grepl("Industrial 64", HOUdiff$Current_La) | grepl("Industrial 65", HOUdiff$Current_La) | grepl("Industrial 66", HOUdiff$Current_La) | 
                                           grepl("Industrial 67", HOUdiff$Current_La) | grepl("Industrial 68", HOUdiff$Current_La) | grepl("Industrial 69", HOUdiff$Current_La) | 
                                           grepl("Industrial 70", HOUdiff$Current_La) | grepl("Industrial 71", HOUdiff$Current_La) | grepl("Industrial 72", HOUdiff$Current_La) | 
                                           grepl("Industrial 73", HOUdiff$Current_La) | grepl("Industrial 74", HOUdiff$Current_La) | grepl("Industrial 75", HOUdiff$Current_La) | 
                                           grepl("Industrial 76", HOUdiff$Current_La) | grepl("Industrial 77", HOUdiff$Current_La) | grepl("Industrial 78", HOUdiff$Current_La) | 
                                           grepl("Industrial 79", HOUdiff$Current_La) | grepl("Industrial 80", HOUdiff$Current_La) | grepl("Industrial 81", HOUdiff$Current_La) | 
                                           grepl("Industrial 82", HOUdiff$Current_La) | grepl("Industrial 83", HOUdiff$Current_La) | grepl("Industrial 84", HOUdiff$Current_La) | 
                                           grepl("Industrial 85", HOUdiff$Current_La) | grepl("Industrial 86", HOUdiff$Current_La) | grepl("Industrial 87", HOUdiff$Current_La) | 
                                           grepl("Industrial 89", HOUdiff$Current_La) | grepl("Industrial 90", HOUdiff$Current_La) | grepl("Industrial 92", HOUdiff$Current_La) | 
                                           grepl("Industrial 95", HOUdiff$Current_La) | grepl("Industrial 100", HOUdiff$Current_La) |
                                           
                                           grepl("Warehouse 51", HOUdiff$Current_La) | grepl("Warehouse 52", HOUdiff$Current_La) | grepl("Warehouse 53", HOUdiff$Current_La) | 
                                           grepl("Warehouse 54", HOUdiff$Current_La) | grepl("Warehouse 55", HOUdiff$Current_La) | grepl("Warehouse 56", HOUdiff$Current_La) | 
                                           grepl("Warehouse 57", HOUdiff$Current_La) | grepl("Warehouse 58", HOUdiff$Current_La) | grepl("Warehouse 59", HOUdiff$Current_La) | 
                                           grepl("Warehouse 60", HOUdiff$Current_La) | grepl("Warehouse 61", HOUdiff$Current_La) | grepl("Warehouse 62", HOUdiff$Current_La) | 
                                           grepl("Warehouse 63", HOUdiff$Current_La) | grepl("Warehouse 64", HOUdiff$Current_La) | grepl("Warehouse 65", HOUdiff$Current_La) | 
                                           grepl("Warehouse 66", HOUdiff$Current_La) | grepl("Warehouse 67", HOUdiff$Current_La) | grepl("Warehouse 68", HOUdiff$Current_La) | 
                                           grepl("Warehouse 69", HOUdiff$Current_La) | grepl("Warehouse 70", HOUdiff$Current_La) | grepl("Warehouse 71", HOUdiff$Current_La) | 
                                           grepl("Warehouse 72", HOUdiff$Current_La) | grepl("Warehouse 73", HOUdiff$Current_La) | grepl("Warehouse 74", HOUdiff$Current_La) | 
                                           grepl("Warehouse 75", HOUdiff$Current_La) | grepl("Warehouse 76", HOUdiff$Current_La) | grepl("Warehouse 77", HOUdiff$Current_La) | 
                                           grepl("Warehouse 78", HOUdiff$Current_La) | grepl("Warehouse 79", HOUdiff$Current_La) | grepl("Warehouse 80", HOUdiff$Current_La) | 
                                           grepl("Warehouse 81", HOUdiff$Current_La) | grepl("Warehouse 82", HOUdiff$Current_La) | grepl("Warehouse 83", HOUdiff$Current_La) | 
                                           grepl("Warehouse 84", HOUdiff$Current_La) | grepl("Warehouse 85", HOUdiff$Current_La) | grepl("Warehouse 86", HOUdiff$Current_La) | 
                                           grepl("Warehouse 87", HOUdiff$Current_La) | grepl("Warehouse 88", HOUdiff$Current_La) | grepl("Warehouse 89", HOUdiff$Current_La) | 
                                           grepl("Warehouse 90", HOUdiff$Current_La) | grepl("Warehouse 91", HOUdiff$Current_La) | grepl("Warehouse 92", HOUdiff$Current_La) | 
                                           grepl("Warehouse 94", HOUdiff$Current_La) | grepl("Warehouse 96", HOUdiff$Current_La) | grepl("Warehouse 97", HOUdiff$Current_La) | 
                                           grepl("Warehouse 98", HOUdiff$Current_La) | grepl("Warehouse 100", HOUdiff$Current_La), 4, #industrial
                                          5))) #all other classes

#Simplify LU by merging polygons with matching codes
zonHOUf = HOUdiff %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#Add new column to parks to match reclassified vector (clean up other columns)
parkHOU$sLANDUSE = 3
parkHOU = parkHOU[,(names(parkHOU) %in% c("sLANDUSE","ParkID"))] #Keep only sLANDUSE, ParkID, and geometry

#Remove unused vectors for for memory conservation
rm(zonHOU)
rm(HOUint)
rm(boundHOU)
rm(HOUdiff)
gc()

#Add the two vectors together
zonHOUfin = bind_rows(zonHOUf,parkHOU)
#st_is_valid(zonHOUfin)

#simplify parks
zonHOUfin = zonHOUfin %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#zonHOUfin = st_collection_extract(zonHOUfin, "POLYGON") #change to the correct format for export

#Check that it worked as expected by plotting
palette = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00")
colors = palette[as.numeric(zonHOUfin$sLANDUSE)]
plot(zonHOUfin, col = colors, max.plot = 1)

# ----Indianapolis----
unique(INDdiff$USE14_CODE)

#Reclassify LU vector with parks clipped out
INDdiff$sLANDUSE = ifelse(INDdiff$USE14_CODE == '11' | INDdiff$USE14_CODE == '12' | INDdiff$USE14_CODE == '13' | INDdiff$USE14_CODE == '14' |
                            INDdiff$USE14_CODE == '15' | INDdiff$USE14_CODE == '16', 1, #single-family residential
                          ifelse(INDdiff$USE14_CODE == '17' | INDdiff$USE14_CODE == '18' | INDdiff$USE14_CODE == '19' | INDdiff$USE14_CODE == '20', 2, #multi-family residential
                                 ifelse(INDdiff$USE14_CODE == '41' | INDdiff$USE14_CODE == '42', 4, #industrial
                                        5))) #all other classes

#Simplify LU by merging polygons with matching codes
zonINDf = INDdiff %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#plot to see how it looks
palette = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00")
colors = palette[as.numeric(zonINDf$sLANDUSE)]
#plot(zonINDf, col = colors, max.plot = 1)

#Remove unused vectors for for memory conservation
rm(zonIND)
rm(INDint)
rm(boundIND)
rm(INDdiff)
gc()

#Add new column to parks to match reclassified vector (clean up other columns)
parkIND$sLANDUSE = 3
parkIND = parkIND[,(names(parkIND) %in% c("sLANDUSE","ParkID"))] #Keep only sLANDUSE, ParkID, and geometry

#Add the two vectors together
zonINDfin = bind_rows(zonINDf,parkIND)

#simplify parks
zonINDfin = zonINDfin %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

zonINDfin = st_collection_extract(zonINDfin, "POLYGON") #change to the correct format for export

#Check that it worked as expected by plotting
colors = palette[as.numeric(zonINDfin$sLANDUSE)]
plot(zonINDfin, col = colors, max.plot = 1)

# ----Jacksonville----
unique(JACdiff$LABEL)

JACdiff$sLANDUSE = ifelse(JACdiff$LABEL == 'LDR' | JACdiff$LABEL == 'RR', 1, #single-family residential
                          ifelse(JACdiff$LABEL == 'HDR' | JACdiff$LABEL == 'MDR' , 2, #multi-family residential
                                 ifelse(JACdiff$LABEL == 'HI'  | JACdiff$LABEL == 'LI'  | JACdiff$LABEL == 'WD/WR' , 4, #industrial
                                        5))) #all other classes

#Simplify LU by merging polygons with matching codes
zonJACf = JACdiff %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#Add new column to parks to match reclassified vector (clean up other columns)
parkJAC$sLANDUSE = 3
parkJAC = parkJAC[,(names(parkJAC) %in% c("sLANDUSE","ParkID"))] #Keep only sLANDUSE, ParkID, and geometry

#Remove unused vectors for for memory conservation
rm(zonJAC)
rm(JACint)
rm(boundJAC)
rm(JACdiff)
gc()

#Add the two vectors together
zonJACfin = bind_rows(zonJACf,parkJAC)
#st_is_valid(zonJACfin)

#simplify parks
zonJACfin = zonJACfin %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#Check that it worked as expected by plotting
palette = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00")
colors = palette[as.numeric(zonJACfin$sLANDUSE)]
plot(zonJACfin, col = colors, max.plot = 1)

# ----Los Angeles----
unique(LAXdiff$LU19_CLASS)

LAXdiff$sLANDUSE = ifelse(LAXdiff$LU19 == 1110 | LAXdiff$LU19 == 1111 | LAXdiff$LU19 == 1112 | LAXdiff$LU19 == 1113 | LAXdiff$LU19 == 1130 |
                            LAXdiff$LU19 == 1131 | LAXdiff$LU19 == 1132 | LAXdiff$LU19 == 1150, 1, #single-family residential
                          ifelse(LAXdiff$LU19 == 1120 | LAXdiff$LU19 == 1121 | LAXdiff$LU19 == 1122 | LAXdiff$LU19 == 1123 |
                                   LAXdiff$LU19 == 1124 | LAXdiff$LU19 == 1125 | LAXdiff$LU19 == 1150, 2, #multi-family residential
                                 ifelse(LAXdiff$LU19 == 1231 | LAXdiff$LU19 == 1300 | LAXdiff$LU19 == 1310 | LAXdiff$LU19 == 1311 | 
                                          LAXdiff$LU19 == 1312 | LAXdiff$LU19 == 1313 | LAXdiff$LU19 == 1314 | LAXdiff$LU19 == 1320 |
                                          LAXdiff$LU19 == 1321 | LAXdiff$LU19 == 1322 | LAXdiff$LU19 == 1323 | LAXdiff$LU19 == 1324 |
                                          LAXdiff$LU19 == 1325 | LAXdiff$LU19 == 1330 | LAXdiff$LU19 == 1331 | LAXdiff$LU19 == 1332 |
                                          LAXdiff$LU19 == 1340 | LAXdiff$LU19 == 1500, 4, #industrial
                                        5))) #all other classes

#Simplify LU by merging polygons with matching codes
zonLAXf = LAXdiff %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#Check that it worked as expected by plotting
palette = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00")
colors = palette[as.numeric(zonLAXf$sLANDUSE)]
plot(zonLAXf, col = colors, max.plot = 1)

#remove NA (sLANDUSE = NA)
zonLAXf2 = na.omit(zonLAXf)

#Add new column to parks to match reclassified vector (clean up other columns)
parkLAX$sLANDUSE = 3
parkLAX = parkLAX[,(names(parkLAX) %in% c("sLANDUSE","ParkID"))] #Keep only sLANDUSE, ParkID, and geometry

#Remove unused vectors for for memory conservation
rm(zonLAX)
rm(LAXint)
rm(boundLAX)
rm(LAXdiff)
gc()

#Add the two vectors together
zonLAXfin = bind_rows(zonLAXf2,parkLAX)

#simplify parks
zonLAXfin = zonLAXfin %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#Check that it worked as expected by plotting
palette = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00")
colors = palette[as.numeric(zonLAXfin$sLANDUSE)]
plot(zonLAXfin, col = colors, max.plot = 1)

# ----New York----
unique(NYCdiff$BldgClass)

NYCdiff$sLANDUSE = ifelse(grepl("^A", NYCdiff$BldgClass) | NYCdiff$BldgClass == 'CM' | NYCdiff$BldgClass == 'Z0', 1, #single-family residential
                          ifelse(grepl("^B", NYCdiff$BldgClass) | NYCdiff$BldgClass == 'C0' | NYCdiff$BldgClass == 'C1'| NYCdiff$BldgClass == 'C2' |
                                   NYCdiff$BldgClass == 'C3'| NYCdiff$BldgClass == 'C4' | NYCdiff$BldgClass == 'C5'| NYCdiff$BldgClass == 'C6'| 
                                   NYCdiff$BldgClass == 'C7'| NYCdiff$BldgClass == 'C8'| NYCdiff$BldgClass == 'C9'| NYCdiff$BldgClass == 'CB'| 
                                   NYCdiff$BldgClass == 'CC' | grepl("^D", NYCdiff$BldgClass) | NYCdiff$BldgClass == 'G0' | NYCdiff$BldgClass == 'R1' |
                                   NYCdiff$BldgClass == 'R2' | NYCdiff$BldgClass == 'R3' | NYCdiff$BldgClass == 'R3' | NYCdiff$BldgClass == 'R4' |
                                   NYCdiff$BldgClass == 'R6' | NYCdiff$BldgClass == 'R9' | grepl("^S", NYCdiff$BldgClass) , 2, #multi-family residential
                                 ifelse(grepl("^E", NYCdiff$BldgClass) | grepl("^F", NYCdiff$BldgClass) | NYCdiff$BldgClass == 'RW', 4, #industrial
                                        5))) #all other classes

#Remove extra columns to see if NYC diff worked
NYCdiffe = NYCdiff[,(names(NYCdiff) %in% c("sLANDUSE","BldgClass","LandUse"))]

#Simplify LU by merging polygons with matching codes
zonNYCf = NYCdiff %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#Add new column to parks to match reclassified vector (clean up other columns)
parkNYC$sLANDUSE = 3
parkNYC = parkNYC[,(names(parkNYC) %in% c("sLANDUSE","ParkID"))] #Keep only sLANDUSE, ParkID, and geometry

#Remove unused vectors for for memory conservation
rm(zonNYC)
rm(NYCint)
rm(boundNYC)
rm(NYCdiffe)
gc()

#Add the two vectors together
zonNYCfin = bind_rows(zonNYCf,parkNYC)
#st_is_valid(zonCHIfin)

#simplify parks
zonNYCfin = zonNYCfin %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#Check that it worked as expected by plotting
palette = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00")
colors = palette[as.numeric(zonNYCfin$sLANDUSE)]
plot(zonNYCfin, col = colors, max.plot = 1)

# ----Phoenix----
unique(PHXdiff$LONG_DISPL)

#Reclassify LU vector with parks clipped out
PHXdiff$sLANDUSE = ifelse(PHXdiff$LONG_DISPL == 'Single Family Low Density' | PHXdiff$LONG_DISPL == 'Single Family Medium Density' | PHXdiff$LONG_DISPL == 'Single Family High Density', 1, #single-family residential
                          ifelse(PHXdiff$LONG_DISPL == 'Multi Family', 2, #multi-family residential
                                 ifelse(PHXdiff$LONG_DISPL == 'Industrial' | PHXdiff$LONG_DISPL == 'Other Employment', 4, #industrial
                                        5))) #all other classes

#Simplify LU by merging polygons with matching codes
zonPHXf = PHXdiff %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#plot to see how it looks
palette = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00")
colors = palette[as.numeric(zonPHXf$sLANDUSE)]
plot(zonPHXf, col = colors, max.plot = 1)

#Add new column to parks to match reclassified vector (clean up other columns)
parkPHX$sLANDUSE = 3
parkPHX = parkPHX[,(names(parkPHX) %in% c("sLANDUSE","ParkID"))] #Keep only sLANDUSE, ParkID, and geometry

#Remove unused vectors for for memory conservation
rm(zonPHX)
rm(PHXint)
rm(boundPHX)
rm(PHXdiff)
gc()

#Add the two vectors together
zonPHXfin = bind_rows(zonPHXf,parkPHX)

#simplify parks
zonPHXfin = zonPHXfin %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#Check that it worked as expected by plotting
colors = palette[as.numeric(zonPHXfin$sLANDUSE)]
plot(zonPHXfin, col = colors, max.plot = 1)

# ----Portland----
unique(PORdiff$zone_cd)

#Reclassify LU vector with parks clipped out
PORdiff$sLANDUSE = ifelse(PORdiff$zone_cd == 'R2.5' | PORdiff$zone_cd == 'R5' | PORdiff$zone_cd == 'R7' | PORdiff$zone_cd == 'R10' |
                            PORdiff$zone_cd == 'R20' | PORdiff$zone_cd == 'RF' | PORdiff$zone_cd == 'RMP' , 1, #single-family residential
                          ifelse(PORdiff$zone_cd == 'RM1' | PORdiff$zone_cd == 'RM2' | PORdiff$zone_cd == 'RM3' | 
                                   PORdiff$zone_cd == 'RM4' | PORdiff$zone_cd == 'RX', 2, #multi-family residential
                                 ifelse(PORdiff$zone_cd == 'IG1' | PORdiff$zone_cd == 'IG2' | PORdiff$zone_cd == 'IH', 4, #industrial
                                        5))) #all other classes

#Simplify LU by merging polygons with matching codes
zonPORf = PORdiff %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#plot to see how it looks
palette = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00")
colors = palette[as.numeric(zonPORf$sLANDUSE)]
plot(zonPORf, col = colors, max.plot = 1)

#Add new column to parks to match reclassified vector (clean up other columns)
parkPOR$sLANDUSE = 3
parkPOR = parkPOR[,(names(parkPOR) %in% c("sLANDUSE","ParkID"))] #Keep only sLANDUSE, ParkID, and geometry

#Remove unused vectors for for memory conservation
rm(zonPOR)
rm(PORint)
rm(boundPOR)
rm(PORdiff)
gc()

#Add the two vectors together
zonPORfin = bind_rows(zonPORf,parkPOR)

#simplify parks
zonPORfin = zonPORfin %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#Check that it worked as expected by plotting
colors = palette[as.numeric(zonPORfin$sLANDUSE)]
plot(zonPORfin, col = colors, max.plot = 1)

# ----Saint Louis----
#Reclassify LU vector with parks clipped out
STLdiff$sLANDUSE = ifelse(STLdiff$LAYER == 'A', 1, #single-family residential
                         ifelse(STLdiff$LAYER == 'B' | STLdiff$LAYER == 'C' | STLdiff$LAYER == 'D' | STLdiff$LAYER == 'E', 2, #multi-family residential
                                ifelse(STLdiff$LAYER == 'J', 4, #industrial
                                      5))) #all other classes

#Simplify LU by merging polygons with matching codes
zonSTLf = STLdiff %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#STLdiff = group_by(STLdiff, sLANDUSE)
#zonSTLf = aggregate(STLdiff,by = "sLANDUSE",do_union = T)
  
#plot to see how it looks
palette = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00")
colors = palette[as.numeric(zonSTLf$sLANDUSE)]
#plot(zonSTLf, col = colors, max.plot = 1)

#Add new column to parks to match reclassified vector (clean up other columns)
parkSTL$sLANDUSE = 3
parkSTL = parkSTL[,(names(parkSTL) %in% c("sLANDUSE","ParkID"))] #Keep only sLANDUSE, ParkID, and geometry

#Remove unused vectors for for memory conservation
rm(zonSTL)
rm(STLint)
rm(boundSTL)
rm(STLdiff)
gc()

#Add the two vectors together
zonSTLfin = bind_rows(zonSTLf,parkSTL)
st_is_valid(zonSTLfin)

#simplify parks
zonSTLfin = zonSTLfin %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

zonSTLfin = st_collection_extract(zonSTLfin, "POLYGON") #change to the correct format for export

#Check that it worked as expected by plotting
colors = palette[as.numeric(zonSTLfin$sLANDUSE)]
plot(zonSTLfin, col = colors, max.plot = 1)

# ----Seattle----
unique(SEAdiff$CLASS_DESC)

#Reclassify LU vector with parks clipped out
SEAdiff$sLANDUSE = ifelse(SEAdiff$CLASS_DESC == 'Neighborhood Residential', 1, #single-family residential
                          ifelse(SEAdiff$CLASS_DESC == 'Multi-Family' | SEAdiff$CLASS_DESC == 'Master Planned Community' | SEAdiff$CLASS_DESC == 'Multi-Family/Residential-Commercial', 2, #multi-family residential
                                 ifelse(SEAdiff$CLASS_DESC == 'Manufacturing/Industrial', 4, #industrial
                                        5))) #all other classes

#Simplify LU by merging polygons with matching codes
zonSEAf = SEAdiff %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#Add new column to parks to match reclassified vector (clean up other columns)
parkSEA$sLANDUSE = 3
parkSEA = parkSEA[,(names(parkSEA) %in% c("sLANDUSE","ParkID"))] #Keep only sLANDUSE, ParkID, and geometry

#Remove unused vectors for for memory conservation
rm(zonSEA)
rm(SEAint)
rm(boundSEA)
rm(SEAdiff)
gc()

#Add the two vectors together
zonSEAfin = bind_rows(zonSEAf,parkSEA)
st_is_valid(zonSEAfin)

#simplify parks
zonSEAfin = zonSEAfin %>%
  dplyr::group_by(sLANDUSE) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

#Check that it worked as expected by plotting
palette = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00")
colors = palette[as.numeric(zonSEAfin$sLANDUSE)]
plot(zonSEAfin, col = colors, max.plot = 1)

# STEP 4 -----------------------------------------------
# Calculate the proportion of each LU class for Table 2

# ----Chicago----
#Add area column
zonCHIfin$area = st_area(zonCHIfin)

#Create variable that is the total area
CHIarea = sum(zonCHIfin$area)

#Create proportion column
zonCHIfin$prop = (zonCHIfin$area / CHIarea) * 100

# ----Houston----
#Add area column
zonHOUfin$area = st_area(zonHOUfin)

#Create variable that is the total area
HOUarea = sum(zonHOUfin$area)

#Create proportion column
zonHOUfin$prop = (zonHOUfin$area / HOUarea) * 100

# ----Indianapolis----
#Add area column
zonINDfin$area = st_area(zonINDfin)

#Create variable that is the total area
INDarea = sum(zonINDfin$area)

#Create proportion column
zonINDfin$prop = (zonINDfin$area / INDarea) * 100

# ----Jacksonville----
#Add area column
zonJACfin$area = st_area(zonJACfin)

#Create variable that is the total area
JACarea = sum(zonJACfin$area)

#Create proportion column
zonJACfin$prop = (zonJACfin$area / JACarea) * 100

# ----Los Angeles----
#Add area column
zonLAXfin$area = st_area(zonLAXfin)

#Create variable that is the total area
LAXarea = sum(zonLAXfin$area)

#Create proportion column
zonLAXfin$prop = (zonLAXfin$area / LAXarea) * 100

# ----New York----
#remove NAs (sLANDUSE = NA)
zonNYCfin = na.omit(zonNYCfin)

#Add area column
zonNYCfin$area = st_area(zonNYCfin)

#Create variable that is the total area
NYCarea = sum(zonNYCfin$area)

#Create proportion column
zonNYCfin$prop = (zonNYCfin$area / NYCarea) * 100

# ----Phoenix----
#Add area column
zonPHXfin$area = st_area(zonPHXfin)

#Create variable that is the total area
PHXarea = sum(zonPHXfin$area)

#Create proportion column
zonPHXfin$prop = (zonPHXfin$area / PHXarea) * 100

# ----Portland----
#Add area column
zonPORfin$area = st_area(zonPORfin)

#Create variable that is the total area
PORarea = sum(zonPORfin$area)

#Create proportion column
zonPORfin$prop = (zonPORfin$area / PORarea) * 100

# ----Saint Louis----
#Add area column
zonSTLfin$area = st_area(zonSTLfin)

#Create variable that is the total area
STLarea = sum(zonSTLfin$area)

#Create proportion column
zonSTLfin$prop = (zonSTLfin$area / STLarea) * 100

# ----Seattle----
#remove water (sLANDUSE = NA)
zonSEAfin = na.omit(zonSEAfin)

#Add area column
zonSEAfin$area = st_area(zonSEAfin)

#Create variable that is the total area
SEAarea = sum(zonSEAfin$area)

#Create proportion column
zonSEAfin$prop = (zonSEAfin$area / SEAarea) * 100

# STEP 5 -----------------------------------------------
# Export reclassified LU shapefiles

st_write(zonCHIfin,'./Data/ZoningLUClipped/Chicago/ChicagoLUTPL_5classes.shp',driver="ESRI Shapefile")
st_write(zonHOUfin,'./Data/ZoningLUClipped/Houston/HoustonLUTPL_5classes.shp',driver="ESRI Shapefile")
st_write(zonINDfin,'./Data/ZoningLUClipped/Indianapolis/IndianapolisLUTPL_5classes.shp',driver="ESRI Shapefile")
st_write(zonJACfin,'./Data/ZoningLUClipped/Jacksonville/JacksonvilleLUTPL_5classes.shp',driver="ESRI Shapefile")
st_write(zonLAXfin,'./Data/ZoningLUClipped/LosAngeles/LosAngelesLUTPL_5classes.shp',driver="ESRI Shapefile")
st_write(zonNYCfin,'./Data/ZoningLUClipped/NewYork/NewYorkLUTPL_5classes.shp',driver="ESRI Shapefile")
st_write(zonPHXfin,'./Data/ZoningLUClipped/Phoenix/PhoenixLUTPL_5classes.shp',driver="ESRI Shapefile")
st_write(zonPORfin,'./Data/ZoningLUClipped/Portland/PortlandLUTPL_5classes.shp',driver="ESRI Shapefile")
st_write(zonSTLfin,'./Data/ZoningLUClipped/SaintLouis/SaintLouisLUTPL_5classes.shp',driver="ESRI Shapefile")
st_write(zonSEAfin,'./Data/ZoningLUClipped/Seattle/SeattleLUTPL_5classes.shp',driver="ESRI Shapefile")

# STEP 6 -----------------------------------------------
# Count unique classes for Table 1

#----Chicago----
length(unique(zonCHI$LANDUSE)) #56

#----Houston----
#come back to
#unique(zonHOU$Label_Curr) 
#length(unique(zonHOU$Label_Curr))

#create list of all unique fields
list = unique(zonHOU$Current_La)

#split multi-part fields (before % sign)
split = strsplit(list, split = "%")

#keep only the unique values and sort
flatten = unlist(split)
uflat = unique(flatten)

library(stringr)
uflats = str_trim(uflat, "left")

uflatss = sort(unique(uflats))
print(uflatss)
tail(uflatss, n = 350)

#----Indianapolis----
length(unique(zonIND$USE14_CODE)) #37

# ----Jacksonville----
length(unique(zonJAC$LABEL)) #19

# ----Los Angeles----
length(unique(zonLAX$LU19_CLASS)) #21

# ----New York----
length(unique(zonNYC$BldgClass)) #212

# ----Phoenix----
length(unique(zonPHX$LONG_DISPL)) #34

# ----Portland----
#First, select only "zon_typ" == "base" (not overlay)
zonPOR = zonPOR[zonPOR$zon_typ == 'base',]

length(unique(zonPOR$zone_cd)) #31

# ----Saint Louis----
length(unique(zonSTL$LAYER)) #14

# ----Seattle----
length(unique(zonSEA$CLASS_DESC)) #10

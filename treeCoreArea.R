#Michelle Stuhlmacher
#2024.02.01

#GOAL: Determine the core patches and then match them up with the corresponding zoning/LU class

#STEPS:
#1) Import data and packages
#2) Run spatailize lsm to create a raster with area values and run get patches to create unique ids
#3) Match zoning shp with core area raster
#   -filter area raster to only include patches greater than 1 ha
#   -mask unique id raster with filtered area raster to remove patches less than 1 ha
#   -do some sort of intersection with the filtered unique id raster where all pixel values
#    that intersect with a given class are matched with that LU
#4) Format results and export (create Figure 2)

# STEP 1 -----------------------------------------------
#set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/TreeLocations")

#Import libraries
library(raster)
library(landscapemetrics)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(scales)

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
zonIND = st_read('./Data/ZoningLUClipped/Indianapolis/IndianapolisLUTPL_5classes.shp')
zonJAC = st_read('./Data/ZoningLUClipped/Jacksonville/JacksonvilleLUTPL_5classes.shp')
zonLAX = st_read('./Data/ZoningLUClipped/LosAngeles/LosAngelesLUTPL_5classes.shp')
zonNYC = st_read('./Data/ZoningLUClipped/NewYork/NewYorkLUTPL_5classes.shp')
zonPHX = st_read('./Data/ZoningLUClipped/Phoenix/PhoenixLUTPL_5classes.shp')
zonPOR = st_read('./Data/ZoningLUClipped/Portland/PortlandLUTPL_5classes.shp')
zonSTL = st_read('./Data/ZoningLUClipped/SaintLouis/SaintLouisLUTPL_5classes.shp')
zonSEA = st_read('./Data/ZoningLUClipped/Seattle/SeattleLUTPL_5classes.shp')

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

# STEP 2 -----------------------------------------------
#Run spatailize lsm to create a raster with the core areas

#----Chicago----
# #Make sure projections match
# zon = st_transform(zonCHI, crs(treeCHI))
# bounds = st_transform(boundCHI, crs(treeCHI))
# 
# #Process raster to select only the tree pixels
# cropCHI = crop(treeCHI,bounds)
# maskCHI = mask(cropCHI,bounds)
# treeCHIonly = mask(maskCHI,maskCHI,maskvalue = 0,updatevalue = NA)
# #plot(treeCHIonly)
# #plot(bounds,add = T)
# 
# #remove unneeded layers to free up memory
# rm(cropCHI)
# rm(maskCHI)
# rm(treeCHI)
# rm(zonCHI)
# rm(boundCHI)
# gc()
# 
# #Run the spatialize
# pAreatreeOnly8 = spatialize_lsm(treeCHIonly,what = "lsm_p_area", directions = 8)
# plot(pAreatreeOnly8$layer_1$lsm_p_area) #Check the results
# pAreaRaster8 = pAreatreeOnly8$layer_1$lsm_p_area #Extract the raster
# 
# #Get unique IDs for patches
# patches8 = get_patches(treeCHIonly, directions = 8, return_raster = T)
# patchID8 = patches8$layer_1$class_1

#----Houston----
#Make sure projections match
zon = st_transform(zonHOU, crs(treeHOU))
bounds = st_transform(boundHOU, crs(treeHOU))

#Process raster to select only the tree pixels
cropHOU = crop(treeHOU,bounds)
maskHOU = mask(cropHOU,bounds)
treeHOUonly = mask(maskHOU,maskHOU,maskvalue = 0,updatevalue = NA)
#plot(treeHOUonly)
#plot(bounds,add = T)

#remove unneeded layers to free up memory
rm(cropHOU)
rm(maskHOU)
rm(treeHOU)
rm(zonHOU)
rm(boundHOU)
gc()

#Run the spatialize
pAreatreeOnly8 = spatialize_lsm(treeHOUonly,what = "lsm_p_area", directions = 8)
plot(pAreatreeOnly8$layer_1$lsm_p_area) #Check the results
pAreaRaster8 = pAreatreeOnly8$layer_1$lsm_p_area #Extract the raster

#Get unique IDs for patches
patches8 = get_patches(treeHOUonly, directions = 8, return_raster = T)
patchID8 = patches8$layer_1$class_1

# #Houston has the same number of >1ha patches for parks and industrial. 
# 
# #Testing that this is also true with tabular data to make sure it's not an error
# patchAreaHOU = calculate_lsm(treeHOUonly, what = c('lsm_p_area'),verbose=T,directions = 8)
# patchAreaHOU %>% 
#   filter(value > 1) #number of patches larger than 1ha

#----Indianapolis----
# #Make sure projections match
# zon = st_transform(zonIND, crs(treeIND))
# bounds = st_transform(boundIND, crs(treeIND))
# 
# #Process raster to select only the tree pixels
# cropIND = crop(treeIND,bounds)
# maskIND = mask(cropIND,bounds)
# treeINDonly = mask(maskIND,maskIND,maskvalue = 0,updatevalue = NA)
# #plot(treeINDonly)
# #plot(bounds,add = T)
# 
# #remove unneeded layers to free up memory
# rm(cropIND)
# rm(maskIND)
# rm(treeIND)
# rm(zonIND)
# rm(boundIND)
# gc()
# 
# #Run the spatialize
# pAreatreeOnly8 = spatialize_lsm(treeINDonly,what = "lsm_p_area", directions = 8)
# plot(pAreatreeOnly8$layer_1$lsm_p_area) #Check the results
# pAreaRaster8 = pAreatreeOnly8$layer_1$lsm_p_area #Extract the raster
# 
# #Get unique IDs for patches
# patches8 = get_patches(treeINDonly, directions = 8, return_raster = T)
# patchID8 = patches8$layer_1$class_1

#----Jacksonville----
# #Make sure projections match
# zon = st_transform(zonJAC, crs(treeJAC))
# bounds = st_transform(boundJAC, crs(treeJAC))
# 
# #Process raster to select only the tree pixels
# cropJAC = crop(treeJAC,bounds)
# maskJAC = mask(cropJAC,bounds)
# treeJAConly = mask(maskJAC,maskJAC,maskvalue = 0,updatevalue = NA)
# #plot(treeJAConly)
# #plot(bounds,add = T)
# 
# #remove unneeded layers to free up memory
# rm(cropJAC)
# rm(maskJAC)
# rm(treeJAC)
# rm(zonJAC)
# rm(boundJAC)
# gc()
# 
# #Run the spatialize
# pAreatreeOnly8 = spatialize_lsm(treeJAConly,what = "lsm_p_area", directions = 8)
# plot(pAreatreeOnly8$layer_1$lsm_p_area) #Check the results
# pAreaRaster8 = pAreatreeOnly8$layer_1$lsm_p_area #Extract the raster
# 
# #Get unique IDs for patches
# patches8 = get_patches(treeJAConly, directions = 8, return_raster = T)
# patchID8 = patches8$layer_1$class_1

#----Los Angeles----
# #Make sure projections match
# zon = st_transform(zonLAX, crs(treeLAX))
# bounds = st_transform(boundLAX, crs(treeLAX))
# 
# #Process raster to select only the tree pixels
# cropLAX = crop(treeLAX,bounds)
# maskLAX = mask(cropLAX,bounds)
# treeLAXonly = mask(maskLAX,maskLAX,maskvalue = 0,updatevalue = NA)
# #plot(treeLAXonly)
# #plot(bounds,add = T)
# 
# #remove unneeded layers to free up memory
# rm(cropLAX)
# rm(maskLAX)
# rm(treeLAX)
# rm(zonLAX)
# rm(boundLAX)
# gc()
# 
# #Run the spatialize
# pAreatreeOnly8 = spatialize_lsm(treeLAXonly,what = "lsm_p_area", directions = 8)
# plot(pAreatreeOnly8$layer_1$lsm_p_area) #Check the results
# pAreaRaster8 = pAreatreeOnly8$layer_1$lsm_p_area #Extract the raster
# 
# #Get unique IDs for patches
# patches8 = get_patches(treeLAXonly, directions = 8, return_raster = T)
# patchID8 = patches8$layer_1$class_1

#----New York----
# #Make sure projections match
# zon = st_transform(zonNYC, crs(treeNYC))
# bounds = st_transform(boundNYC, crs(treeNYC))
# 
# #Process raster to select only the tree pixels
# cropNYC = crop(treeNYC,bounds)
# maskNYC = mask(cropNYC,bounds)
# treeNYConly = mask(maskNYC,maskNYC,maskvalue = 0,updatevalue = NA)
# #plot(treeNYConly)
# #plot(bounds,add = T)
# 
# #remove unneeded layers to free up memory
# rm(cropNYC)
# rm(maskNYC)
# rm(treeNYC)
# rm(zonNYC)
# rm(boundNYC)
# gc()
# 
# #Run the spatialize
# pAreatreeOnly8 = spatialize_lsm(treeNYConly,what = "lsm_p_area", directions = 8)
# plot(pAreatreeOnly8$layer_1$lsm_p_area) #Check the results
# pAreaRaster8 = pAreatreeOnly8$layer_1$lsm_p_area #Extract the raster
# 
# #Get unique IDs for patches
# patches8 = get_patches(treeNYConly, directions = 8, return_raster = T)
# patchID8 = patches8$layer_1$class_1

#----Phoenix----
# #Make sure projections match
# zon = st_transform(zonPHX, crs(treePHX))
# bounds = st_transform(boundPHX, crs(treePHX))
# 
# #Process raster to select only the tree pixels
# cropPHX = crop(treePHX,bounds)
# maskPHX = mask(cropPHX,bounds)
# treePHXonly = mask(maskPHX,maskPHX,maskvalue = 0,updatevalue = NA)
# #plot(treePHXonly)
# #plot(bounds,add = T)
# 
# #remove unneeded layers to free up memory
# rm(cropPHX)
# rm(maskPHX)
# rm(treePHX)
# rm(zonPHX)
# rm(boundPHX)
# gc()
# 
# #Run the spatialize
# pAreatreeOnly8 = spatialize_lsm(treePHXonly,what = "lsm_p_area", directions = 8)
# plot(pAreatreeOnly8$layer_1$lsm_p_area) #Check the results
# pAreaRaster8 = pAreatreeOnly8$layer_1$lsm_p_area #Extract the raster
# 
# #Get unique IDs for patches
# patches8 = get_patches(treePHXonly, directions = 8, return_raster = T)
# patchID8 = patches8$layer_1$class_1

#----Portland----
# #Make sure projections match
# zon = st_transform(zonPOR, crs(treePOR))
# bounds = st_transform(boundPOR, crs(treePOR))
# 
# #Process raster to select only the tree pixels
# cropPOR = crop(treePOR,bounds)
# maskPOR = mask(cropPOR,bounds)
# treePORonly = mask(maskPOR,maskPOR,maskvalue = 0,updatevalue = NA)
# #plot(treePORonly)
# #plot(bounds,add = T)
# 
# #remove unneeded layers to free up memory
# rm(cropPOR)
# rm(maskPOR)
# rm(treePOR)
# rm(zonPOR)
# rm(boundPOR)
# gc()
# 
# #Run the spatialize
# pAreatreeOnly8 = spatialize_lsm(treePORonly,what = "lsm_p_area", directions = 8)
# plot(pAreatreeOnly8$layer_1$lsm_p_area) #Check the results
# pAreaRaster8 = pAreatreeOnly8$layer_1$lsm_p_area #Extract the raster
# 
# #Get unique IDs for patches
# patches8 = get_patches(treePORonly, directions = 8, return_raster = T)
# patchID8 = patches8$layer_1$class_1

#----Saint Louis----
#Make sure projections match
zon = st_transform(zonSTL, crs(treeSTL))
bounds = st_transform(boundSTL, crs(treeSTL))

#Process raster to select only the tree pixels
cropSTL = crop(treeSTL,bounds)
maskSTL = mask(cropSTL,bounds)
treeSTLonly = mask(maskSTL,maskSTL,maskvalue = 0,updatevalue = NA)
#plot(treeSTLonly)
#plot(bounds,add = T)

#remove unneeded layers to free up memory
rm(cropSTL)
rm(maskSTL)
rm(treeSTL)
rm(zonSTL)
rm(boundSTL)
gc()

#Run the spatialize
pAreatreeOnly8 = spatialize_lsm(treeSTLonly,what = "lsm_p_area", directions = 8)
plot(pAreatreeOnly8$layer_1$lsm_p_area) #Check the results
pAreaRaster8 = pAreatreeOnly8$layer_1$lsm_p_area #Extract the raster

#Get unique IDs for patches
patches8 = get_patches(treeSTLonly, directions = 8, return_raster = T)
patchID8 = patches8$layer_1$class_1

#----Seattle----
# #Make sure projections match
# zon = st_transform(zonSEA, crs(treeSEA))
# bounds = st_transform(boundSEA, crs(treeSEA))
# 
# #Process raster to select only the tree pixels
# cropSEA = crop(treeSEA,bounds)
# maskSEA = mask(cropSEA,bounds)
# treeSEAonly = mask(maskSEA,maskSEA,maskvalue = 0,updatevalue = NA)
# plot(treeSEAonly)
# #plot(bounds,add = T)
# 
# #remove unneeded layers to free up memory
# rm(cropSEA)
# rm(maskSEA)
# rm(treeSEA)
# rm(zonSEA)
# rm(boundSEA)
# gc()
# 
# #Run the spatialize
# pAreaSEAtreeOnly8 = spatialize_lsm(treeSEAonly,what = "lsm_p_area", directions = 8)
# plot(pAreaSEAtreeOnly8$layer_1$lsm_p_area) #Check the results
# pAreaRasterSEA8 = pAreaSEAtreeOnly8$layer_1$lsm_p_area #Extract the raster
# 
# # pAreaSEAtreeOnly4 = spatialize_lsm(treeSEAonly,what = "lsm_p_area", directions = 4) #direction 4 does not work
# # plot(pAreaSEAtreeOnly4$layer_1$lsm_p_area) #Check the results
# # pAreaRasterSEA4 = pAreaSEAtreeOnly4$layer_1$lsm_p_area #Extract the raster
# 
# # #Run the tabular
# # patchAreaSEA4 = calculate_lsm(treeSEAonly, what = c('lsm_p_area'),verbose=T,directions = 4)
# 
# #Get unique IDs for patches
# patches8 = get_patches(treeSEAonly, directions = 8, return_raster = T)
# patchID8 = patches8$layer_1$class_1

# STEP 3 -----------------------------------------------
#Match zoning shp with core area raster

#Getting extent errors on the mask below, so first will compute the minimal extent and crop both rasters
xmin <- max(bbox(as(pAreaRaster8,"Raster"))[1,1], bbox(as(patchID8,"Raster"))[1,1])
xmax <- min(bbox(as(pAreaRaster8,"Raster"))[1,2], bbox(as(patchID8,"Raster"))[1,2])  
ymin <- max(bbox(as(pAreaRaster8,"Raster"))[2,1], bbox(as(patchID8,"Raster"))[2,1])  
ymax <- min(bbox(as(pAreaRaster8,"Raster"))[2,2], bbox(as(patchID8,"Raster"))[2,2]) 

newextent=c(xmin, xmax, ymin, ymax)
pAreaRaster8C = crop(pAreaRaster8, newextent)
patchID8C = crop(patchID8, newextent)

# FILTER 1 HECTARE PATCHES ----
#Filter to only the patches that are greater than 1 hectare
pAreaRaster8C[pAreaRaster8C < 1] <- NA

#Mask id raster with filtered area raster to remove patches less than 1 ha
patchIDMasked1ha = mask(x = as(patchID8C,"Raster"), mask = as(pAreaRaster8C,"Raster"),updatevalue = NA,inverse=F)
plot(patchIDMasked1ha)

#Intersect the filtered unique id raster with LU vector to link unique ID pixel values with the classes they overlap
#https://www.neonscience.org/resources/learning-hub/tutorials/extract-values-rasters-r
pixelID1ha = raster::extract(patchIDMasked1ha,zon)

#Clean up list to get the count of the unique cluster ids
listCount1ha = list(LU1 = length(na.omit(unique(pixelID1ha[[1]]))),
                    LU2 = length(na.omit(unique(pixelID1ha[[2]]))),
                    LU3 = length(na.omit(unique(pixelID1ha[[3]]))),
                    LU4 = length(na.omit(unique(pixelID1ha[[4]]))),
                    LU5 = length(na.omit(unique(pixelID1ha[[5]]))))

countDF1haT = as.data.frame(listCount1ha) #create dataframe from list
countDF1ha = as.data.frame(t(countDF1haT)) #transpose dataframe
colnames(countDF1ha)[1] = "numPatch1ha" #give column name
countDF1ha$sLANDUSE = c(1,2,3,4,5) #add LU column for merge

#Join back up with zoning shape
zon1ha = merge(zon,countDF1ha,by='sLANDUSE')

# FILTER 12 HECTARE PATCHES ----
#Filter to only the patches that are greater than 12 hectares
pAreaRaster8C[pAreaRaster8C < 12] <- NA

#Mask id raster with filtered area raster to remove patches less than 1 ha
patchIDMasked12ha = mask(x = as(patchID8C,"Raster"), mask = as(pAreaRaster8C,"Raster"),updatevalue = NA,inverse=F)
plot(patchIDMasked12ha)

#Intersect the filtered unique id raster with LU vector to link unique ID pixel values with the classes they overlap
pixelID12ha = raster::extract(patchIDMasked12ha,zon)

#Clean up list to get the count of the unique cluster ids
listCount12ha = list(LU1 = length(na.omit(unique(pixelID12ha[[1]]))),
                     LU2 = length(na.omit(unique(pixelID12ha[[2]]))),
                     LU3 = length(na.omit(unique(pixelID12ha[[3]]))),
                     LU4 = length(na.omit(unique(pixelID12ha[[4]]))),
                     LU5 = length(na.omit(unique(pixelID12ha[[5]]))))

countDF12haT = as.data.frame(listCount12ha) #create dataframe from list
countDF12ha = as.data.frame(t(countDF12haT)) #transpose dataframe
colnames(countDF12ha)[1] = "numPatch12ha" #give column name
countDF12ha$sLANDUSE = c(1,2,3,4,5) #add LU column for merge

#Join back up with zoning shape
zon12ha = merge(zon1ha,countDF12ha,by='sLANDUSE')

# STEP 4 -----------------------------------------------
#Format as a faceted (by city) and grouped (by patch size) bar chart

#Import dataframe
DF = read.csv('./Results/PatchCount/PatchCount_2024.02.16.csv')

#Remove "other"
DF = DF[DF$Zone != "Other", ]

#Plot vertically
ggplot(DF, aes(fill=Patch.Size, y=Number.of.Patches, x=Zone)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~City, scales = "free_y", nrow = 5) +
  scale_x_discrete(labels = label_wrap(6)
                   ,limits = c("Single Family Residential", "Multi Family Residential", "Parks", "Industrial")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.text.x = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank()) +
  #scale_fill_manual(values=c("#1b9e77","#d95f02")) +
  scale_fill_manual(values=c("#2CA25F","#00441B")) +
  labs(fill = "Patch Size", y = "Number of Patches")

#test out what a plot would look like if only using >12 ha patches for writing the results section
DF12 = DF[DF$Patch.Size == '>12 ha', ]

ggplot(DF12, aes(y=Number.of.Patches, x=Zone)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~City, scales = "free_y", nrow = 5) +
  scale_x_discrete(labels = label_wrap(6)
                   ,limits = c("Single Family Residential", "Multi Family Residential", "Parks", "Industrial")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.text.x = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank())

### ----Version with "other" for appendix----
#Import dataframe
DF = read.csv('./Results/PatchCount/PatchCount_2024.02.16.csv')

#Plot vertically
ggplot(DF, aes(fill=Patch.Size, y=Number.of.Patches, x=Zone)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~City, scales = "free_y", nrow = 5) +
  scale_x_discrete(labels = label_wrap(6)
                   ,limits = c("Single Family Residential", "Multi Family Residential", "Parks", "Industrial", "Other")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.text.x = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank()) +
  scale_fill_manual(values=c("#1b9e77","#d95f02")) +
  labs(fill = "Patch Size", y = "Number of Patches")

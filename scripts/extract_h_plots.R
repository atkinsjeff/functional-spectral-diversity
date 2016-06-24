###
# this code takes a shapefile polygon extent and a directory containging
# a bunch of NEON H5 files and returns the flightlines that INTERSECT the 
# polygon boundary
###

# library(devtools)
## install from github
# install_github("lwasser/neon-aop-package/neonAOP")


# load libraries
library(sp)
library(rgeos)
library(rhdf5)
library(neonAOP)

extract_plt<-function(clipFile) {

########## Inputs #####

# shapefile path and name
# shape.path <- "NEONdata/D17-California/SJER/vector_data"
# clipFile <- "sjer_clip_extent"


########## Inputs #####

### TEAK Clip
# #the name of the site
# site <- "TEAK"
# domain <- "D17"
# fullDomain <- "D17-California"
# level <- "L1"
# dataType <- "Spectrometer"
# level <- paste0(site,"_L1")
# year <- "2013"
# productType <- paste0(site,"_", dataType)
# dataProduct <- "Reflectance"
# clipFile <- "TEAK_plot"

### SOAP Clip
# # the name of the site
# site <- "SOAP"
# domain <- "D17"
# fullDomain <- "D17-California"
# level <- "L1"
# dataType <- "Spectrometer"
# level <- paste0(site,"_L1")
# year <- "2013"
# productType <- paste0(site,"_", dataType)

##### OSBS Clip
# the name of the site
site <- "OSBS"
domain <- "D03"
fullDomain <- "D03-Florida"
level <- "L1"
dataType <- "lidar"
level <- paste0(site,"_L1")
year <- "2014"
productType <- paste0(site,"_Spectrometer")
dataProduct <- "Reflectance"

### SOAP Clip
# the name of the site
# site <- "SOAP"
# domain <- "D17"
# fullDomain <- "D17-Florida"
# level <- "L1"
# dataType <- "Spectrometer"
# level <- paste0(site,"_L1")
# year <- "2013"
# productType <- paste0(site,"_", dataType)
# dataProduct <- "Reflectance"

### SJER clip
# site <- "SJER"
# domain <- "D17"
# fullDomain <- "D17-California"
# level <- "L1"
# dataType <- "Spectrometer"
# level <- paste0(site, "_L1")
# year <- "2013"
# productType <- paste0(site,"_", dataType)

#####
dataProduct <- "Reflectance"
productType <- paste0(site,"_Spectrometer")
level <- paste0(site,"_L1")

#clipFile <- paste0(site,"_crop")

drivePath <- "D:/"
#driveName <- "AOP-NEON1-4"

dataDir <- file.path(drivePath,
                     domain,
                     site, year, level, productType, dataProduct)
#dataDir <- paste0(dataDir)
# get a list of all files in the dir
h5.files <- list.files(dataDir, pattern = '\\.h5$', full.names = TRUE)


#### Import shapefile ####
# # import shapefile
# clipFilePath <- file.path("../NEONdata", fullDomain, site, "vector_data")
# clipFile <- paste0(site,"_crop")
# clip.polygon <- readOGR(clipFilePath,
#                         clipFile)


#####
## functions
## Check Extent Function ####
# this function below checks to see if a raster falls within a spatial extent
# inputs: raster file to check, clipShp (spatial )
checkExtent <- function(aRaster, clipShp){
  # create polygon extent assign CRS to extent 
  h5.extent.sp <- as(h5.extent, "SpatialPolygons")
  # note this is ASSUMING both the extent and the h5 file are in the same CRS
  crs(rasterExtPoly) <-  crs(clip.polygon)
  
  # check to see if the polygons overlap
  # return a boolean (1= the raster contains pixels within the extent, 0 it doesn't)
  return(gIntersects(h5.extent.sp, clip.polygon))
}


# initalize counter and list object
recordRaster <- NA
i <- 0

# the loop below returns a LIST of the files that have overlapping extent
for(afile in h5.files){
  # get extent of h5 file
  h5Extent <- create_extent(afile)
  # turn into polygon extent object
  h5.poly <- as(h5Extent, "SpatialPolygons")
  # this is assuming both are in the same CRS!
  crs(h5.poly) <-  crs(clip.polygon)
  
  # check to see if the polygons overlap
  if(gIntersects(h5.poly, clip.polygon)){
    i <- i+1
    recordRaster[i] <- afile
  } else {
    print("not in")
  }
}

recordRaster
}

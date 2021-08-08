#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Big Prairie Creek Watershed Delineation
#Date: 7/29/2021
#Coder: Nate Jones (cnjones7@ua.edu)
#Purpose: Delineate watershed for Big Prairie Creek Watershed Delineation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Required WBT Package -- See download directions here: https://github.com/giswqs/whiteboxR

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Setup workspace -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear Memory
remove(list = ls())

#Load relevant packages
library(tidyverse) #join the cult
library(whitebox)
library(sf)
library(raster)
library(stars)
library(mapview)
library(htmlwidgets)

#Define data directories
data_dir<-"data//I_data//"
scratch_dir<-"data//II_work//"
output_dir<-"data//III_output//"

#Define data inputs
dem<-raster(paste0(data_dir,"ned30m32087.tif"))
crop<-st_read(paste0(data_dir,"crop.shp"))
lulc<-raster(paste0(data_dir,"nlcd_al_utm16.tif"))
pnts<-st_read(paste0(scratch_dir, "pnts.shp"))
  # tibble(
  #   y=c(32.582970,32.619280,32.577040), 
  #   x=c(-87.520600, -87.511670, -87.531086)) %>% 
  # st_as_sf(., 
  #          coords = c("x", "y"), 
  #          crs = '+proj=longlat +datum=WGS84 +no_defs') %>% 
  # st_transform(., crs = st_crs(dem@crs))

#Crop raster data to area of interest
dem<-crop(dem, crop)
lulc<-crop(lulc, crop)

#Export intermediate files
writeRaster(dem, paste0(scratch_dir,"dem.tif"))
writeRaster(lulc, paste0(scratch_dir,"lulc.tif"))

#Plot for Funzies
mapview(dem)+mapview(pnts)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Create analysis function ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Function
fun<-function(
  shed_id,     #watershed id
  scratch_dir, #Temp folder to store intermediates
  dem,         #Input digital elevation model
  pp          #Watershed pourpoint
){
  
#2.1 Initial dem processin -----------------------------------------------------
#Smooth DEM
  wbt_gaussian_filter(
    input = "dem.tif", 
    output = "dem_smoothed.tif",
    wd = scratch_dir)

#breach depressions
wbt_breach_depressions(
  dem =    "dem_smoothed.tif",
  output = "dem_breached.tif",
  fill_pits = F,
  wd = scratch_dir)

#Flow direction raster
wbt_d8_pointer(
  dem= "dem_breached.tif",
  output ="fdr.tif",
  wd = scratch_dir
)

#Flow accumulation raster
wbt_d8_flow_accumulation(
  input = "dem_breached.tif",
  out_type= "cells",
  output = "fac.tif",
  wd = scratch_dir
)

#Create Stream Layer
wbt_extract_streams(
  flow_accum = "fac.tif",
  output = "stream.tif",
  threshold = 1000,
  wd = scratch_dir
)
  
#2.2 Delineate Watershed -------------------------------------------------------
#Paste point points in scratch dir
st_write(pp, paste0(scratch_dir,"pp.shp"), delete_dsn = T)

#Snap pour point
wbt_jenson_snap_pour_points(
  pour_pts = "pp.shp", 
  streams = "stream.tif",
  snap_dist = 1000,
  output =  "snap.shp",
  wd= scratch_dir)

#Delineate watersheds
wbt_watershed(
  d8_pntr = "fdr.tif",
  pour_pts = "snap.shp", 
  output = "sheds.tif" ,
  wd=scratch_dir)

#load watershed raster into R env
sheds<-raster(paste0(scratch_dir,"sheds.tif"))

#Convert raster to vector
sheds<- sheds %>% st_as_stars() %>% st_as_sf(., merge = TRUE)

#Define Shed_ID
sheds$shed_id<-shed_id

#Define area [ha]
sheds$area_ha<-st_area(sheds, byid=T)
sheds$area_ha<-as.numeric(paste0(sheds$area_ha))/10000
sheds$area_ha<-round(sheds$area_ha, 0)

#Remove 'snippets"
sheds<-sheds %>% filter(area_ha>90)

#2.3 Zonal Statistics ----------------------------------------------------------
#Crop rasters to watershed area
lulc<-crop(lulc, sheds)
lulc<-mask(lulc, sheds)

#Estimate zonal stats
output<-rasterToPoints(lulc) %>% #Convert raster cells to pnts
  #Convert to tibble to tidy
  as_tibble() %>% 
  #Subset to larger lulc class
  mutate(class = substr(nlcd_al_utm16,1,1)) %>% 
  #summarie by nlcd class
  group_by(class) %>% 
  summarise(area = n()) %>% 
  #Estimate area in ha
  mutate(area = area*(30^2)/10000) %>% 
  #pivot wider
  pivot_wider(names_from=class, values_from=area) %>% 
  #Add watershed_ID
  mutate(shed_id=shed_id)

#Rename cols
colnames(output)<-c("open_water", "developed","forest","shrubland","herbaceous","agriculture","wetlands", "shed_id")

  
#2.4 Export --------------------------------------------------------------------
sheds<-left_join(sheds, output)
sheds
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3: Watershed Analysis ----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Run watershed analysis
shed_1<-fun("shed_1", scratch_dir, dem, pnts[1,])
shed_2<-fun("shed_2", scratch_dir, dem, pnts[2,])
shed_3<-fun("shed_3", scratch_dir, dem, pnts[3,])
shed_4<-fun("shed_4", scratch_dir, dem, pnts[4,])
shed_5<-fun("shed_5", scratch_dir, dem, pnts[5,])

#Combine outputs
sheds<-bind_rows(shed_1, shed_2, shed_3, shed_4, shed_5)

#Collect lulc data
lulc_area<-sheds %>% st_drop_geometry()
write_csv(lulc_area, paste0(output_dir,"lulc_area.csv"))

#Export shapefiles for mapping in ArcGIS
st_write(shed_3, paste0(output_dir,"shed.shp"))
st_write(shed_2, paste0(output_dir,"shed_ag.shp"))
st_write(shed_4, paste0(output_dir,"shed_forested.shp"))

#Export stream 
streams<-raster(paste0(scratch_dir,"stream.tif"))
streams<-crop(streams, shed_3)
streams<-mask(streams, shed_3)
writeRaster(streams, paste0(output_dir,"streams.tif"))

#Create map
mapviewOptions(fgb = FALSE)
m<-mapview(dem)+
  mapview(shed_3, col.regions="red")+
  mapview(shed_1, col.regions="green")+
  mapview(shed_2, col.regions="blue")
m

#export map
mapshot(m, "docs//sheds.html", selfcontained=T)

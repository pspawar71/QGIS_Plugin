##FIELDimageR=group
##mosaic_layer=raster
##index_layer=raster
##mask_layer_name=string HUE
##mask_crop_values=number 0
##crop_above=boolean TRUE
##output_mask=output raster
##output_mosaic=output raster

# Get input parameters from QGIS
library(terra)

# Load the raster layer
mosaic <- rast(mosaic_layer)
n_bands <- nlyr(mosaic)
last_band <- mosaic[[nlyr(mosaic)]]
if (all(values(last_band) %in% c(0, 255))) {
  mosaic <- mosaic[[1:(n_bands - 1)]]
  print("The last band was removed because it was transparent band.")
} else {
  print("The last band is not transparent.")
}
print(mosaic)
index<-rast(index_layer)
print(index)
indice<-mask_layer_name
print(indice)
nBand <- nlyr(index)
if(nBand==1){
  names(index[[1]])<-as.character(indice)
}
if (any(!indice%in%names(index))) {
  stop("Selected indices are not available in FIELDimageR")
}
if(crop_above){
mask <- ifel(index[[indice]]> mask_crop_values,NA,1)
}
if(!crop_above){
mask <- ifel(index[[indice]]> mask_crop_values,1,NA)
}
names(mask)<-'mask'
if (!terra::ext(mask) == terra::ext(mosaic)) {
  rastaligned <- terra::resample(mask, mosaic)
  mosaic <- terra::mask(mosaic, rastaligned)
} else {
  rastaligned <- mask
  mosaic <- terra::mask(mosaic, mask)
}

# Write the raster to disk
output_mask <- rastaligned 
output_mosaic<-mosaic

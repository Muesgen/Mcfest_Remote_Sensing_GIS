root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

#List files in the data aerial org folder
ls <- list.files(paste0(envrmt$path_data_aerial_org), pattern = glob2rx("*.tif"))

#Read Images
imagelist <- lapply(paste0(envrmt$path_data_aerial_org, ls), raster::brick)

#Check projections of images
pro <- lapply(imagelist, crs)
if (length(unique(pro)) == 1){
  print("all projections are equal")
} else {
  print("at least one image has not the same projection")
}

#Read edited Shapefile
abt<- readOGR(paste0(envrmt$path_data_mof, "uwcAbteilung.shp"),
              layer= ogrListLayers(paste0(envrmt$path_data_mof, "uwcAbteilung.shp")))

#Check projections of Shapefile
crs(abt)

#Cropping relevant images (picture one and two do not overlap with the bounding box - no cropping required)
cropped <- lapply(imagelist[3:length(imagelist)], raster::crop, abt)

#(Optional) write out cropped raster
for (l in cropped[1:length(cropped)]) {
  writeRaster(l, filename = paste0(envrmt$path_data_aerial_processed, "cropped", 
                                   substr(gsub("[.]", "_", names(l)[1]), 1, 15),
                                   substr(gsub("[.]", "_", names(l)[1]), 18, 19), ".tif"), overwrite=TRUE)
}

#Merging the "stripes" toghether with the actual images
imagelist_3_4_cm <- raster::mosaic(cropped$`3`,cropped$`4`, fun="min", filename=paste0(envrmt$path_data_aerial_processed, "b3_4_cm.tif"))
imagelist_5_6_cm <- raster::mosaic(cropped$`5`, cropped$`6`, fun="min", filename=paste0(envrmt$path_data_aerial_processed, "b5_6_cm.tif"))

#creating one mosaic
img <- raster::merge(imagelist_3_4_cm, imagelist_5_6_cm, cropped$`7`, cropped$`8`, filename=paste0(envrmt$path_data_aerial_processed, "img.tif"))

#Plot final image and shapefile togehter
plotRGB(brick(paste0(envrmt$path_data_aerial_processed, "img.tif")))
plot(abt, col = adjustcolor("#92DDFF", alpha.f = 0.5), add=TRUE)
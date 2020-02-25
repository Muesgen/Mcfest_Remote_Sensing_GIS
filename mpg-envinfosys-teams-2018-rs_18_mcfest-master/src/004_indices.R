root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

#img <- raster::raster(paste0(envrmt$path_data_aerial_processed, "img.tif")) #read original image

#img_res <- raster::resample(img, chm, method = "bilinear") #resample original image to match spatial resolution of crown model (0.5 meters)

#raster::writeRaster(img_res, filename = paste0(envrmt$path_data_aerial_processed, "img_res.tif"), overwrite = TRUE) #write resampled image; for sake of simplicity I refer to it further along as "img"

img <- raster::stack(paste0(envrmt$path_data_aerial_processed, "img_res.tif"))

#calculate rgb indices as shown in 002_functions.R
#indices <- rgbIndices(img, rgbi = c("VVI", "TGI", "GLI", "CIVE", "VARI", "ExGR", "VEG", 
 #                                   "NGRDI", "NDTI", "CI", "BI", "SI", "HI", "ExG", "COM", "CEV", "mcfesti"))

#Write indices to physical files
#writeRaster(indices, filename = paste0(envrmt$path_data_aerial_processed, names(indices), "_index.tif"), bylayer=TRUE, overwrite = TRUE)


indices <- raster::stack(paste0(envrmt$path_data_aerial_processed,
                       list.files(paste0(envrmt$path_data_aerial_processed), pattern=glob2rx("*_index.tif"))))
indices <- raster::stack(indices, img[[1]], img[[2]], img[[3]]) #load indices from files and add red, green and blue bands from the resampled rgb image


####LIDAR Indices#### #Not yet...
#chm <- raster::raster(paste0(envrmt$path_data_lidar, "canopy.tif"))
#lindices <- raster::terrain(x = chm, opt = c("slope", "aspect", "TPI", "TRI", "roughness", "flowdir"), unit = "radians")





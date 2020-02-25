root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

#read all train shapes
files <- list.files(paste0(envrmt$path_data_training), pattern = "*.shp", full.names = TRUE)
shapes <- lapply(files, raster::shapefile)
#set id and reproject to raster projection
for (h in 1:length(shapes)){
  shapes[[h]]@data$id <- 1:length(shapes[[h]]@data$id)
  shapes[[h]] <- sp::spTransform(shapes[[h]], crs(raster::stack(paste0(envrmt$path_data_aerial_processed_selection, "BI_index_norm.tif"))))
}

#determine aera of trainshapes in propotion to the entire waldorte layer
waldo <- raster::shapefile(paste0(envrmt$path_data_mof, "uwcWaldorte.shp"))
ar <- c()
for (i in seq(10)){
  temp <- sum(area(shapes[[i]]))
  ar <- c(ar, temp)
}
sum(ar)/sum(area(waldo))

#extract values to list
rasfiles <- list.files(paste0(envrmt$path_data_aerial_processed_selection), pattern = "*.tif", full.names = TRUE)

ext_tab <- extraction(shp = shapes, rasfiles = rasfiles)


saveRDS(object = ext_tab, file = paste0(envrmt$path_data_training, "ext_tab_all_2.rds"))
traindat <- do.call("rbind", ext_tab)
traindat <- traindat[, c(1, 57, 56, 2:55)] #Reorder the segment ID and the tree type to the first columns
traindat <- na.omit(traindat) #remove NA's introduced by entall in some BU polygons
write.table(traindat, file = paste0(envrmt$path_data_training, "traindat_all_2.csv"), row.names = FALSE, 
            dec = ".", sep = ";")

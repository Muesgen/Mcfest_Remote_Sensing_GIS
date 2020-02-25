root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

####Filter ueber PCA####
#Apply function as written in 002_functions.R to first principal component
# strfil <- filter(pca$map$PC1, fil=c("mean5", "mean15", "mean21", "mean31", "sobel5", "sobel15", "sobel21", "sobel31", 
#                                     "gauss5", "gauss15", "gauss21", "gauss31", "LoG5", "LoG15", "LoG21", "LoG31"))
# for (i in 1:length(names(strfil))){
#   names(strfil[[i]]) <- paste0("pca1_",names(strfil[[i]]))
# }
# writeRaster(strfil, filename = paste0(envrmt$path_data_aerial_processed, names(strfil), "_filter.tif"), bylayer=TRUE, overwrite = TRUE)
strfil <- raster::stack(paste0(envrmt$path_data_aerial_processed,
                        list.files(paste0(envrmt$path_data_aerial_processed), pattern=glob2rx("pca1*_filter.tif"))))

#Apply function as written in 002_functions.R to first principal component
# strfil2 <- filter(pca$map$PC2, fil=c("mean5", "mean15", "mean21", "mean31", "sobel5", "sobel15", "sobel21", "sobel31", 
#                                      "gauss5", "gauss15", "gauss21", "gauss31", "LoG5", "LoG15", "LoG21", "LoG31"))
# for (i in 1:length(names(strfil2))){
#   names(strfil2[[i]]) <- paste0("pca2_",names(strfil2[[i]]))
# }
# writeRaster(strfil2, filename = paste0(envrmt$path_data_aerial_processed, names(strfil2), "_filter.tif"), bylayer=TRUE, overwrite = TRUE)

strfil2 <- raster::stack(paste0(envrmt$path_data_aerial_processed,
                       list.files(paste0(envrmt$path_data_aerial_processed), pattern=glob2rx("pca2*_filter.tif"))))

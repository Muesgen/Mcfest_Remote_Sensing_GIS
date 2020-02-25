root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

mof <- uavRst::make_lidr_catalog(paste0(envrmt$path_data_lidar_org_extend), chunksize = 500, 
                                 chunkbuffer = 10, cores = 4)
								 
crs(mof) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#Normalize data points (height)
lidR::opt_output_files(mof)<- paste0(envrmt$path_data_lidar_norm, "{ID}_norm")
#mof_norm <- lidR::lasnormalize(mof, lidR::tin())
mof_corrnorm <- uavRst::make_lidr_catalog(paste0(envrmt$path_data_lidar_norm), chunksize = 500,
                                          chunkbuffer = 10, cores = 4)
lidR:::catalog_laxindex(mof_corrnorm)

#reclassify ground points
lidR::opt_output_files(mof_corrnorm)<-paste0(envrmt$path_data_lidar_csf,"{ID}_csf")
#mof_ground_csf <- lidR::lasground(mof_corrnorm, csf())
mof_cat_csf <- uavRst::make_lidr_catalog(paste0(envrmt$path_data_lidar_csf), chunksize = 500, 
                                 chunkbuffer = 10, cores = 4)
lidR:::catalog_laxindex(mof_corrnorm)

######Filter Functions#####
filterz = function(las,minZ = 0, maxZ = 5){
  las = readLAS(las)
  if (is.empty(las)) return(NULL)
  las = lidR::lasfilter(las, Z >=minZ & Z < maxZ)
  grid = lidR::grid_metrics(las, res = 2, .stdmetrics_z)
  grid = grid[[c(1,2,3,6,7)]]
  return(grid)
}
filteri = function(las,minZ = 0, maxZ = 5){
  las = readLAS(las)
  if (is.empty(lidR::lasfilter(las, Z >=minZ & Z < maxZ))) return(NULL)
  las = lidR::lasfilter(las, Z >=minZ & Z < maxZ)
  #grid = lidR::grid_metrics(las, res = 2, stdmetrics_i(Intensity))
  #grid = grid[[c(1,2,3,4)]] #
  return(las)
}


#Read catalog without buffer
mof_cat_csf <- uavRst::make_lidr_catalog(paste0(envrmt$path_data_lidar_csf), chunksize = 500, 
                                         chunkbuffer = 0, cores = 4)

#Read already selected tif to crop lidar metrics
pca <- raster::raster(paste0(envrmt$path_data_aerial_processed_selection, "pca1.tif"))

#####Filter levels#####
#Level 1 0 - 1.5 meters (Krautschicht)
lidR::opt_output_files(mof_cat_csf)<-paste0(envrmt$path_data_lidar_height_level1,"{ID}_level1")
#level1 = lidR::catalog_apply(mof_cat_csf, filteri,0, 1.5)
mof_lev1 <- uavRst::make_lidr_catalog(paste0(envrmt$path_data_lidar_height_level1), chunksize = 500, 
                                      chunkbuffer = 0, cores = 4)
lidR:::catalog_laxindex(mof_lev1)

#Level 2 1.5 - 5 meters (Strauchschicht)
lidR::opt_output_files(mof_cat_csf)<-paste0(envrmt$path_data_lidar_height_level2,"{ID}_level2")
#level2 = lidR::catalog_apply(mof_cat_csf, filteri,1.5, 5)
mof_lev2 <- uavRst::make_lidr_catalog(paste0(envrmt$path_data_lidar_height_level2), chunksize = 500, 
                                      chunkbuffer = 0, cores = 4)
lidR:::catalog_laxindex(mof_lev2)

#Level 3 5 - 15  meters (Stammschicht)
lidR::opt_output_files(mof_cat_csf)<-paste0(envrmt$path_data_lidar_height_level3,"{ID}_level3")
#level3 = lidR::catalog_apply(mof_cat_csf, filteri,5, 15)
mof_lev3 <- uavRst::make_lidr_catalog(paste0(envrmt$path_data_lidar_height_level3), chunksize = 500, 
                                      chunkbuffer = 0, cores = 4)
lidR:::catalog_laxindex(mof_lev3)

#Level 4 15 - 60 meters (Kronenschicht)
lidR::opt_output_files(mof_cat_csf)<-paste0(envrmt$path_data_lidar_height_level4,"{ID}_level4")
#level4 = lidR::catalog_apply(mof_cat_csf, filteri,15, 60)
mof_lev4 <- uavRst::make_lidr_catalog(paste0(envrmt$path_data_lidar_height_level4), chunksize = 500, 
                                      chunkbuffer = 0, cores = 4)
lidR:::catalog_laxindex(mof_lev4)

# #Level 5 20-25 meters
# lidR::opt_output_files(mof_cat_csf)<-paste0(envrmt$path_data_lidar_height_level5,"{ID}_level5")
# #level5 = lidR::catalog_apply(mof_cat_csf, filteri,20, 60)
# mof_lev5 <- uavRst::make_lidr_catalog(paste0(envrmt$path_data_lidar_height_level5), chunksize = 500, 
#                                       chunkbuffer = 0, cores = 4)
# lidR:::catalog_laxindex(mof_lev5)
# 
# #Level 6 25-30 meters
# lidR::opt_output_files(mof_cat_csf)<-paste0(envrmt$path_data_lidar_height_level6,"{ID}_level6")
# #level6 = lidR::catalog_apply(mof_cat_csf, filteri,25,30)
# mof_lev6 <- uavRst::make_lidr_catalog(paste0(envrmt$path_data_lidar_height_level6), chunksize = 500, 
#                                       chunkbuffer = 0, cores = 4)
# lidR:::catalog_laxindex(mof_lev6)
# 
# #Level 7 30-35 meters
# lidR::opt_output_files(mof_cat_csf)<-paste0(envrmt$path_data_lidar_height_level7,"{ID}_level7")
# #level7 = lidR::catalog_apply(mof_cat_csf, filteri,30,50)
# mof_lev7 <- uavRst::make_lidr_catalog(paste0(envrmt$path_data_lidar_height_level7), chunksize = 500, 
#                                       chunkbuffer = 0, cores = 4)
# lidR:::catalog_laxindex(mof_lev7)

#####Entropy#####
#All Levels
entall <- lidR::grid_metrics(mof_cat_csf, res = 2, lidR::entropy(Z, by = 1), start = c(0, 0))
entall <- raster::projectRaster(entall, pca)
entall <- raster::crop(entall, pca)
writeRaster(entall, filename = paste0(envrmt$path_data_lidar_processed_shannon, "entall.tif"), overwrite = TRUE)

#Level 1
ent1 <- lidR::grid_metrics(mof_lev1, res = 2, lidR::entropy(Z, by = 1), start = c(0, 0))
ent1 <- raster::projectRaster(ent1, pca)
ent1 <- raster::crop(ent1, cive)
writeRaster(ent1, filename = paste0(envrmt$path_data_lidar_processed_shannon, "lev1ent.tif"), overwrite = TRUE)

#Level 2
ent2 <- lidR::grid_metrics(mof_lev2, res = 2, lidR::entropy(Z, by = 1), start = c(0, 0))
ent2 <- raster::projectRaster(ent2, pca)
ent2 <- raster::crop(ent2, cive)
writeRaster(ent2, filename = paste0(envrmt$path_data_lidar_processed_shannon, "lev2ent.tif"), overwrite = TRUE)

#Level 3
ent3 <- lidR::grid_metrics(mof_lev3, res = 2, lidR::entropy(Z, by = 1), start = c(0, 0))
ent3 <- raster::projectRaster(ent3, pca)
ent3 <- raster::crop(ent3, cive)
writeRaster(ent3, filename = paste0(envrmt$path_data_lidar_processed_shannon, "lev3ent.tif"), overwrite = TRUE)

#Level 4
ent4 <- lidR::grid_metrics(mof_lev4, res = 2, lidR::entropy(Z, by = 1), start = c(0, 0))
ent4 <- raster::projectRaster(ent4, pca)
ent4 <- raster::crop(ent4, cive)
writeRaster(ent4, filename = paste0(envrmt$path_data_lidar_processed_shannon, "lev4ent.tif"), overwrite = TRUE)


#####Height-Stats#####
lidR::opt_output_files(mof_cat_csf)<-paste0(envrmt$path_data_lidar_processed_zstats,"{ID}_zstats")
zstats <- lidR::grid_metrics(mof_cat_csf,.stdmetrics_z, res = 2, start = c(0,0))
names(zstats) <- c("zmax", "zmean", "zsd", "zskew", "zkurt", "zentropy", "pzabovezmean", "pzabove2", "zq5", "zq10", 
                   "zq15", "zq20", "zq25", "zq30", "zq35", "zq40", "zq45", "zq50", "zq55", "zq60", "zq65", "zq70",
                   "zq75", "zq80", "zq85", "zq90", "zq95", "zpcum1", "zpcum2", "zpcum3", "zpcum4", "zpcum5", "zpcum6",
                   "zpcum7", "zpcum8", "zpcum9")
saveRDS(zstats, paste0(envrmt$path_data_lidar_processed_zstats,"zstats.rds"))
x <- readRDS(paste0(envrmt$path_data_lidar_processed_zstats,"zstats.rds"))

lidR::opt_output_files(mof_cat_csf)<-paste0(envrmt$path_data_lidar_processed,"treestats/{ID}_treestats")
treetstats = lidR::grid_metrics(mof_cat_csf,.stdtreemetrics, res = 2, start = c(0,0))#ReturnNumber not found
writeRaster(treetstats, filename = paste0(envrmt$path_data_lidar_prc,"treestats_allLevels.tif"))

t = lidR::grid_metrics(mof_cat_csf, .stdmetrics, res = 2, start = c(0,0))

#####Number of Returns#####
#All
lidR::opt_output_files(mof_cat_csf)<-paste0(envrmt$path_data_lidar_processed_nreturns,"{ID}_rstats")
returnsAL = lidR::grid_density(mof_cat_csf, res = 2)
writeRaster(returnsAL, filename = paste0(envrmt$path_data_lidar_processed_nreturns,"returns_allLevels.tif"), overwrite = TRUE)

#Level 1
lidR::opt_output_files(mof_lev1)<-paste0(envrmt$path_data_lidar_processed_nreturns_level1,"{ID}_rstats")
lev1 = lidR::grid_density(mof_lev1, res = 2)
lev1 [values(lev1) < 0 | is.na(values(lev1))] = 0
writeRaster(lev1, filename = paste0(envrmt$path_data_lidar_processed_nreturns_level1,"lev1.tif"), overwrite = TRUE)

#Level 2
lidR::opt_output_files(mof_lev2)<-paste0(envrmt$path_data_lidar_processed_nreturns_level2,"{ID}_rstats")
lev2 = lidR::grid_density(mof_lev2, res = 2)
lev2 [values(lev2) < 0 | is.na(values(lev2))] = 0
writeRaster(lev2, filename = paste0(envrmt$path_data_lidar_processed_nreturns_level2,"lev2.tif"), overwrite = TRUE)

#Level 3
lidR::opt_output_files(mof_lev3)<-paste0(envrmt$path_data_lidar_processed_nreturns_level3,"{ID}_rstats")
lev3 = lidR::grid_density(mof_lev3, res = 2)
lev3 [values(lev3) < 0 | is.na(values(lev3))] = 0
writeRaster(lev3, filename = paste0(envrmt$path_data_lidar_processed_nreturns_level3,"lev3.tif"), overwrite = TRUE)

#Level 4
lidR::opt_output_files(mof_lev4)<-paste0(envrmt$path_data_lidar_processed_nreturns_level4,"{ID}_rstats")
lev4 = lidR::grid_density(mof_lev4, res = 2)
lev4 [values(lev4) < 0 | is.na(values(lev4))] = 0
writeRaster(lev4, filename = paste0(envrmt$path_data_lidar_processed_nreturns_level4,"lev4.tif"), overwrite = TRUE)


#####Correcting Stripes#####
lev1_corr <- round((lev1/returnsAL)*100, 0)
lev1_corr[is.na(lev1_corr)] <- 0
writeRaster(lev1_corr, paste0(envrmt$path_data_lidar_processed_nreturns_level1, "lev1_corr.tif"), overwrite = TRUE)

lev2_corr <- round((lev2/returnsAL)*100, 0)
lev2_corr[is.na(lev2_corr)] <- 0
writeRaster(lev2_corr, paste0(envrmt$path_data_lidar_processed_nreturns_level2, "lev2_corr.tif"), overwrite = TRUE)

lev3_corr <- round((lev3/returnsAL)*100, 0)
lev3_corr[is.na(lev3_corr)] <- 0
writeRaster(lev3_corr, paste0(envrmt$path_data_lidar_processed_nreturns_level3, "lev3_corr.tif"), overwrite = TRUE)

lev4_corr <- round((lev4/returnsAL)*100, 0)
lev4_corr[is.na(lev4_corr)] <- 0
writeRaster(lev4_corr, paste0(envrmt$path_data, "lev4_corr.tif"), overwrite = TRUE)

x <- lidR::readLAS(paste0(envrmt$path_data_lidar_org_extend, "U4765632.las"))

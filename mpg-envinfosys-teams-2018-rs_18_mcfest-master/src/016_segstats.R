root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))
#####Statistics per polygon#####
# cent <- rgeos::gCentroid(cseg, byid = TRUE)
# dat <- data.frame(ID=c(1:79990))
# cent <- SpatialPointsDataFrame(coords=cent, data = dat)
# writeOGR(cent, paste0(envrmt$path_data_mof, "centroids.shp"), layer = "centroids", driver = "ESRI Shapefile")
# cseg <- raster::shapefile(paste0(envrmt$path_data_mof, "cseg_corr.shp"))
# props <- read.csv(file = paste0(envrmt$path_data_training, "props_mod9.csv"), sep = ";", dec = ".", header = TRUE)
# zstats <- raster::raster(paste0(envrmt$path_data_lidar_processed_zstats, "zmax.tif"))

###Number of neighbours for each tree polygon####
# csegbuff <- rgeos::gBuffer(cseg, width = .1, byid = TRUE)
# csegcont <- rgeos::gIntersects(spgeom1 = csegbuff, spgeom2 = cseg, byid = TRUE, returnDense = FALSE)
# nseg <- unlist(lapply(csegcont, length))
# cseg@data$neighbour <- (nseg-1)
# writeOGR(cseg, dsn = paste0(envrmt$path_data_mof, "cseg_neigh.shp"), layer = "cseg_neigh",
#          driver = "ESRI Shapefile", overwrite_layer = TRUE)
####Zmax per polygon####
# cseg <- sp::spTransform(cseg, CRSobj = crs(zstats))
# cseg@data$zmax <- 1
# for (z in seq(length(cseg))){
#   cseg@data[z, "zmax"] <- max(unlist(extract(zstats$zmax, cseg[z,])))
#   print(z)
# }
cseg@data$zmax <- neigh@data$zmax
cseg@data$neigh <- neigh@data$neighbour - 1
entall <- readRDS(paste0(envrmt$path_data_training, "entall_extr.rds"))
cseg@data$entall <- entall
writeOGR(cseg, dsn = paste0(envrmt$path_data_mof, "cseg_neigh_zmax.shp"), layer = "cseg_neigh_zmax",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
cseg <- raster::shapefile(paste0(envrmt$path_data_mof, "cseg_neigh_zmax.shp"))
cseg@data <- merge(x = cseg@data, y = props, by.x = "treeID", by.y = "segid", sort = FALSE)

####Entropy per polygon####
# cseg <- raster::shapefile(paste0(envrmt$path_data_mof, "cseg_corr.shp"))
# entall <- raster::raster(paste0(envrmt$path_data_lidar_processed_shannon, "entall.tif"))
# cseg <- sp::spTransform(cseg, CRSobj = crs(entall))
# t <- unlist(lapply(seq(length(cseg)), function(u){
#   tmp <- mean(unlist(raster::extract(entall, cseg[u,])), na.rm=TRUE)
#   print(u)
#   return(tmp)
# }))
# cseg@data$entall <- t
# 
# for (z in seq(length(cseg))){
#   cseg@data[z, "entall"] <- max(unlist(extract(zstats$zmax, cseg[z,])))
#   print(z)
# }

# csegs6 <- raster::shapefile(paste0(envrmt$path_data_mof, "cseg_stats_mod6.shp"))
# csegs7 <- raster::shapefile(paste0(envrmt$path_data_mof, "cseg_stats_mod7.shp"))
# entall <- readRDS(paste0(envrmt$path_data_training, "entall_extr.rds"))
# csegs6$entall <- entall
# csegs7$entall <- entall
# writeOGR(csegs6, dsn = paste0(envrmt$path_data_mof, "cseg_stats_mod6.shp"), layer = "cseg_stats_mod6",
#                   driver = "ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(csegs7, dsn = paste0(envrmt$path_data_mof, "cseg_stats_mod7.shp"), layer = "cseg_stats_mod7",
#          driver = "ESRI Shapefile", overwrite_layer = TRUE)

# cseg@data$zmax <- cseg_height@data$zmax



#####Species detection accurracy#####
# cseg <- raster::shapefile(paste0(envrmt$path_data_mof, "cseg_stats_mod6.shp"))
# metrics <- raster::shapefile(paste0(envrmt$path_data_mof, "hor_p_metrics.shp"))

s <- data.frame(x = c(1:5), y = c("BU", "DGL", "FI", "LA", "EI"))
m <- lapply(seq(nrow(s)), function(h){
  x <- length(raster::intersect(cseg[which(as.character(cseg@data$spec) == s[h, 2]),], metrics[which(metrics@data$FE_DWBA == s[h, 2]),]))
  y <- length(raster::intersect(cseg, metrics[which(metrics@data$FE_DWBA == s[h, 2]),]))
  print(h)
  return(x/y)
})

cseg@data$spec_acc_mod9 <- NA
for (l in seq(nrow(s))){
cseg@data$spec_acc_mod9[which(as.character(cseg@data$spec) == s[l, 2])] <- m[[l]]
print(l)
}

writeOGR(obj = csegs8, dsn = paste0(envrmt$path_data_mof, "cseg_stats_mod8.shp"),
         layer = "cseg_stats_mod8", driver = "ESRI Shapefile", overwrite_layer = TRUE)


#####Accurracy of species classification on each forest section ex. 60 BU's/100 Trees  in a BU section = 60% #####
metrics <- raster::shapefile(paste0(envrmt$path_data_mof, "hor_p_metrics.shp"))
cseg <- raster::shapefile(paste0(envrmt$path_data_mof, "cseg_stats_mod9.shp"))

acc_fs <- lapply(seq(length(metrics)), function(l){
  tem <- as.data.frame(table(raster::intersect(metrics[l,], cseg)@data$spec))
  if (length(tem$Freq[which(tem$Var1 == metrics[l,]$FE_DWBA)]) != 0){
  acc <- tem$Freq[which(tem$Var1 == metrics[l,]$FE_DWBA)] / sum(tem$Freq)
  }
  else {
    acc <- 0
  }
  print(paste(l, acc))
  return(acc)
})
metrics@data$acc_fs_mod9 <- unlist(acc_fs)

writeOGR(metrics, dsn = paste0(envrmt$path_data_mof, "hor_p_metrics.shp"), layer = "hor_p_metrics",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
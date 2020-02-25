root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

# cent <- rgeos::gCentroid(cseg, byid = TRUE)
# dat <- data.frame(ID=c(1:79990))
# cent <- SpatialPointsDataFrame(coords=cent, data = dat)
# writeOGR(cent, paste0(envrmt$path_data_mof, "centroids.shp"), layer = "centroids", driver = "ESRI Shapefile")
cseg <- raster::shapefile(paste0(envrmt$path_data_mof, "cseg_corr.shp"))
cent <- raster::shapefile(paste0(envrmt$path_data_mof, "centroids.shp"))
abt <- raster::shapefile(paste0(envrmt$path_data_mof, "uwcWaldorte.shp"))
abt <- sp::spTransform(abt, crs(cent))
zstats <- readRDS(paste0(envrmt$path_data_lidar_processed_zstats, "zstats.rds"))


#####Centroids per Species#####
bu <- subset(abt, FE_DWBAGRP == "BU")
fi <- subset(abt, FE_DWBAGRP == "FI")
dgl <- subset(abt, FE_DWBAGRP == "DGL")
la <- subset(abt, FE_DWBAGRP == "LA")
ei <- subset(abt, FE_DWBAGRP == "EI")
ges <- subset(abt, FE_DWBAGRP == "BU" | FE_DWBAGRP == "FI"| FE_DWBAGRP == "DGL" | FE_DWBAGRP == "LA" | FE_DWBAGRP == "EI")

#Area in m²
abu <- sum(area(bu))
afi <- sum(area(fi))
adgl <- sum(area(dgl))
ala <- sum(area(la))
aei <- sum(area(ei))
ages <- sum(area(ges))

#####Density in Ha  for each species
ges@data$P_SP_ha[ges@data$FE_DWBAGRP =="BU"] <- (nrow(raster::intersect(cent, bu)@data)/abu)*10000
ges@data$P_SP_ha[ges@data$FE_DWBAGRP =="FI"] <- (nrow(raster::intersect(cent, fi)@data)/afi)*10000
ges@data$P_SP_ha[ges@data$FE_DWBAGRP =="DGL"] <- (nrow(raster::intersect(cent, dgl)@data)/adgl)*10000
ges@data$P_SP_ha[ges@data$FE_DWBAGRP =="LA"] <- (nrow(raster::intersect(cent, la)@data)/ala)*10000
ges@data$P_SP_ha[ges@data$FE_DWBAGRP =="EI"] <- (nrow(raster::intersect(cent, ei)@data)/aei)*10000

pges <- (nrow(raster::intersect(cent, ges)@data)/ages)*10000

#####Centroids per Forest Section####
ges@data$P_FS_ha <- 1
for (i in seq(length(ges))){
ges@data[i,"P_FS_ha"] <- nrow(raster::intersect(cent, ges[i,])@data)/area(ges[i,])*10000
}
writeOGR(ges, dsn = paste0(envrmt$path_data_mof, "hor_p_metrics.shp"), layer = "hor_p_metrics", driver = "ESRI Shapefile")

####Ratio of soil pixels to crown pixels per Species####
int_bu <- raster::intersect(cseg, bu)
ges@data$P_RSC_sp[ges@data$FE_DWBAGRP == "BU"] <- (sum(area(bu)) - sum(area(int_bu))) / sum(area(int_bu))

int_fi <- raster::intersect(cseg, fi)
ges@data$P_RSC_sp[ges@data$FE_DWBAGRP == "FI"] <- (sum(area(fi)) - sum(area(int_fi))) / sum(area(int_fi))

int_dgl <- raster::intersect(cseg, dgl)
ges@data$P_RSC_sp[ges@data$FE_DWBAGRP == "DGL"] <- (sum(area(dgl)) - sum(area(int_dgl))) / sum(area(int_dgl))

int_la <- raster::intersect(cseg, la)
ges@data$P_RSC_sp[ges@data$FE_DWBAGRP == "LA"] <- (sum(area(la)) - sum(area(int_la))) / sum(area(int_la))

int_ei <- raster::intersect(cseg, ei)
ges@data$P_RSC_sp[ges@data$FE_DWBAGRP == "EI"] <- (sum(area(ei)) - sum(area(int_ei))) / sum(area(int_ei))

writeOGR(ges, dsn = paste0(envrmt$path_data_mof, "hor_p_metrics.shp"), layer = "hor_p_metrics", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

####Ratio of soil pixels to crown pixels per Forest Section####
ges@data$P_RSC <- 1
for (l in seq(length(ges))){
  int <- raster::intersect(cseg, ges[l,])
  ges@data[l, "P_RSC"] <- (area(ges[l,]) - sum(area(int))) / sum(area(int))
  print(l)
}

writeOGR(ges, dsn = paste0(envrmt$path_data_mof, "hor_p_metrics.shp"), layer = "hor_p_metrics", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

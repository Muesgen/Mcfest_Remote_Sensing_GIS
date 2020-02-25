root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))


areapredict <- raster::raster(paste0(envrmt$path_data_training, "areapredmod9.tif"))
#areapredict <- raster::projectRaster(areapredict, " +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# cseg <- raster::shapefile(paste0(envrmt$path_data_mof, "cseg55.shp"))
# cseg <- subset(cseg, !treeID %in% c(64894,48007))
# cseg <- sp::spTransform(cseg, crs(areapredict))
# writeOGR(obj = cseg, dsn = paste0(envrmt$path_data_mof, "cseg_corr.shp"), 
#          layer = "cseg_corr", driver = "ESRI Shapefile", overwrite_layer = TRUE)
cseg <- raster::shapefile(paste0(envrmt$path_data_mof, "cseg_corr.shp"))
# cseg_height <- raster::shapefile(paste0(envrmt$path_data_training, "cseg_zmax_neigh.shp"))

# mask <- gdalUtils::gdal_rasterize(src_datasource = paste0(envrmt$path_data_mof, "cseg_corr.shp"),
#                                dst_filename = paste0(envrmt$path_data_training, "rcseg.tif"),
#                                a = "treeID",
#                                tr = c(xres(areapredict), yres(areapredict)),
#                                te = c(xmin(cseg), ymin(cseg), xmax(cseg), ymax(cseg)),
#                                #ts = c(ncol(areapredict), nrow(areapredict)),
#                                l = "cseg_corr", 
#                                #a_srs = "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
#                                output_Raster = TRUE)
rcseg <- raster::raster(paste0(envrmt$path_data_training, "rcseg.tif"))
areapredict <- crop(areapredict, rcseg)

segID <- rcseg[][which(values(rcseg!=0))]
val <- areapredict[][which(values(rcseg!=0))]

extr <- data.frame(segid = segID, type = val)
inf <- table(extr)
props <- data.frame(segid = rownames(inf), BU = inf[,1], DGL = inf[,2], FI = inf[,3], LAR = inf[,4], TEI = inf[,5])
rownames(props) <- 1:nrow(props)
props$sum <- rowSums(props[,2:6])

#Calculate Proportions
pr <- unlist(lapply(seq(nrow(props)), function(d){
  return(max(props[d,2:6])/props$sum[d])
}))
props$prop <- pr

#Add "Winning" type
v <- c("BU", "DGL", "FI", "LA", "EI")
spec <- lapply(seq(nrow(props)), function(i){
  return(v[which(props[i, 2:6] == max(props[i, 2:6]))])
})
len <- unlist(lapply(spec, length))
spec[len!=1] <- "unspec"
spec <- do.call(rbind, spec)
props$spec <- spec[,1]
#45581
props <- props[-which(props$segid %in% c(48007, 64894)),] #correct props
write.table(x = props, file = paste0(envrmt$path_data_training, "props_mod9.csv"), sep = ";", dec = ".")

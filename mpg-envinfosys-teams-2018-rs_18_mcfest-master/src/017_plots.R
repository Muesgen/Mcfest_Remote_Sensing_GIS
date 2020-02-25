root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",	
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")	

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))	

####Confusion Matrix####
library(scales)
library(ggplot2)
ggplotConfusionMatrix <- function(m, mod){
  mytitle <- paste("External Accuracy", percent_format()(m$overall[1]),
                   "Internal Accuracy", percent_format()(max(mod$results$Accuracy)), "\n",
                   "External Kappa", percent_format()(m$overall[2]),
                   "Internal Kappa", percent_format() (max(mod$results$Kappa)))
  
  p <- ggplot(data = as.data.frame(m$table) ,
              aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none", plot.title = element_text(hjust=0.5, face = "bold", size = 20), axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20, margin = margin(t = 10, r = 0, b = 0, l = 0)), 
          axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)), title = element_text(size = 24), 
          panel.background = element_rect(fill = "#d8d8d8", colour = "#d8d8d8", size = 0.5, linetype = "solid"),
          plot.background = element_rect(fill = "#d8d8d8")) +
    ggtitle(mytitle)
  return(p)
}

names <- c("Beech", "Douglas fir", "Spruce", "Larch", "Oak")
mod8 <- readRDS(paste0(envrmt$path_data_training, "mod8.rds"))
conf8 <- readRDS(paste0(envrmt$path_data_training, "confmod8.rds"))
colnames(conf8$table) <- names
rownames(conf8$table) <- names
ggplotConfusionMatrix(conf8, mod8)

mod9 <- readRDS(paste0(envrmt$path_data_training, "mod9.rds"))
conf9 <- readRDS(paste0(envrmt$path_data_training, "confmod9.rds"))
colnames(conf9$table) <- names
rownames(conf9$table) <- names
x <- ggplotConfusionMatrix(conf9, mod9)

png(paste0(envrmt$path_data_plots, "conf8.png"), res=200, width=10, height = 8, units = "in")
print(ggplotConfusionMatrix(conf8, mod8))
dev.off()
png(paste0(envrmt$path_data_plots, "conf9.png"), res=200, width=10, height = 8, units = "in")
print(ggplotConfusionMatrix(conf9, mod9))
dev.off()


####Species Acc####
csegs8 <- raster::shapefile(paste0(envrmt$path_data_mof, "cseg_stats_mod8.shp"))
csegs9 <- raster::shapefile(paste0(envrmt$path_data_mof, "cseg_stats_mod9.shp"))
unique(csegs8@data[which(csegs8@data$spec %in% unique(csegs8@data$spec)[1:5]), c(14, 16)])
unique(csegs9@data[which(csegs9@data$spec %in% unique(csegs9@data$spec)[1:5]), c(16, 17)])


####Var Importance####
mod8 <- readRDS(paste0(envrmt$path_data_training, "mod8.rds"))
conf8 <- readRDS(paste0(envrmt$path_data_training, "confmod8.rds"))
x <- caret::varImp(mod8)
for (i in seq(nrow(x$importance))){
x$importance$mean[i] <- rowMeans(x$importance[i,1:5])
}
write.table(x = x$importance, file = paste0(envrmt$path_data_training , "conf8imp.csv"), sep = ";", dec = ".")


mod9 <- readRDS(paste0(envrmt$path_data_training, "mod9.rds"))
conf9 <- readRDS(paste0(envrmt$path_data_training, "confmod9.rds"))
x <- caret::varImp(mod9)
for (i in seq(nrow(x$importance))){
  x$importance$mean[i] <- rowMeans(x$importance[i,1:5])
}
write.table(x = x$importance, file = paste0(envrmt$path_data_training , "conf9imp.csv"), sep = ";", dec = ".")

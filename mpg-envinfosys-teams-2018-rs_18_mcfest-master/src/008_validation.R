root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

nadellaub <- raster::raster(paste0(envrmt$path_data_lidar_segtest, "testalle.tif"))
nadellaub <- raster::focal(nadellaub, matrix(1/9, nrow = 3, ncol = 3), fun = sum)
# writeRaster(nadellaub, paste0(envrmt$path_data_lidar_segtest_nadel_laub_test, "meannadellaub.tif"), overwrite = TRUE)

#ForestTools
#lin <- function(x){x * 0.06 + 0.5}

x <-0
stats <- list()
for (i in seq(0.0700,0.071, 0.0001)){
  x <- x+1
  stats[[x]] <- ft(raster = nadellaub, mul = i, sum = 0.5, minHeight = 8)
}

mul_x <-c()
for (i in 1:length(stats)){
  mul_x <- c(mul_x, stats[[i]][[1]][[1]])
}

sum_x <-c()
for (i in 1:length(stats)){
  sum_x <- c(sum_x, stats[[i]][[1]][[2]])
}

thup_x <- c()
for (i in 1:length(stats)){
  thup_x <- c(thup_x, stats[[i]][[3]][[4]])
}

acc_x <- c()
for (i in 1:length(stats)){
  acc_x <- c(acc_x, stats[[i]][[2]][[6]])
}

err_x <- c()
for (i in 1:length(stats)){
  err_x <- c(err_x, stats[[i]][[2]][[8]])
}

dev_x <- c()
for (i in 1:length(stats)){
  dev_x <- c(dev_x, stats[[i]][[2]][[9]])
}

hit_x <- c()
for (i in 1:length(stats)){
  hit_x <- c(hit_x, stats[[i]][[2]][[4]])
}

hit_x <- c()
for (i in 1:length(stats)){
  hit_x <- c(hit_x, stats[[i]][[2]][[4]])
}

plotframe <- data.frame(thup_x, acc_x, err_x, dev_x, hit_x, mul_x)
# saveRDS(plotframe, paste0(envrmt$path_data_plots, "plotframe_spec05.rds"))
# saveRDS(plotframe, paste0(envrmt$path_data_plots, "plotframe_range.rds"))
# saveRDS(plotframe, paste0(envrmt$path_data_plots, "plotframe_spec.rds"))
# saveRDS(plotframe, paste0(envrmt$path_data_plots, "plotframe_diff.rds"))
# saveRDS(plotframe, paste0(envrmt$path_data_plots, "plotframe_sum_range.rds"))
# saveRDS(plotframe, paste0(envrmt$path_data_plots, "plotframe_mul_range_sum05.rds"))

#plotframe <- readRDS(paste0(envrmt$path_data_validation, "plotframe_spec05.rds"))

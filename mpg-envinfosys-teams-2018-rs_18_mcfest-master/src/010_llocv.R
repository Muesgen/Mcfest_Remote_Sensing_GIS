root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))


tifs <- lapply(list.files(envrmt$path_data_validation_valtif, pattern = glob2rx("*.tif"), full.names = TRUE), raster)
pts <- readOGR(paste0(envrmt$path_data_validation_valshp, "val_all.shp"), layer = "val_all")
pts <- spTransform(pts, crs(tifs[[1]]))

####ohne1####
tempshp <- raster::intersect(pts, tifs[[1]])
tempshp <- rbind(pts, tempshp)
tempshp <- tempshp[!(duplicated(tempshp@coords) | duplicated(tempshp@coords, fromLast = TRUE)), ]
writeOGR(tempshp, paste0(envrmt$path_data_validation, "tempshp.shp"),
         "tempshp", driver="ESRI Shapefile", overwrite_layer = TRUE)

ohne1 <- raster::merge(tifs[[2]], tifs[[3]], tifs[[4]], tifs[[5]], tifs[[6]])

ohne1stat <- ft(ohne1, mul = 0.07, sum = 0.5, minHeight = 8, 
           points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")

x <-0
stats <- list()
for (i in seq(0.095,0.10, 0.001)){
  x <- x+1
  stats[[x]] <- ft(raster = ohne1, mul = i, sum = 0.5, minHeight = 8, 
                   points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")
  print(x)
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

ohne1test <- ft(tifs[[1]], mul = 0.07, sum = 0.5, minHeight = 8, 
            points = paste0(envrmt$path_data_validation_valshp, "Val_Frauke.shp"), lay = "Val_Frauke")

####ohne2####
tempshp <- raster::intersect(pts, tifs[[2]])
tempshp <- rbind(pts, tempshp)
tempshp <- tempshp[!(duplicated(tempshp@coords) | duplicated(tempshp@coords, fromLast = TRUE)), ]
writeOGR(tempshp, paste0(envrmt$path_data_validation, "tempshp.shp"),
         "tempshp", driver="ESRI Shapefile", overwrite_layer = TRUE)

ohne2 <- raster::merge(tifs[[1]], tifs[[3]], tifs[[4]], tifs[[5]], tifs[[6]])

ohne2stat <- ft(ohne2, mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")

x <-0
stats <- list()
for (i in seq(0.085,0.095, 0.001)){
  x <- x+1
  stats[[x]] <- ft(raster = ohne2, mul = i, sum = 0.5, minHeight = 8, 
                   points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")
  print(x)
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

ohne2test <- ft(tifs[[2]], mul = 0.0876, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation_valshp, "Val_Marvin.shp"), lay = "Val_Marvin")

ohne2test <- ft(tifs[[2]], mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation_valshp, "Val_Marvin.shp"), lay = "Val_Marvin")

####ohne3####
tempshp <- raster::intersect(pts, tifs[[3]])
tempshp <- rbind(pts, tempshp)
tempshp <- tempshp[!(duplicated(tempshp@coords) | duplicated(tempshp@coords, fromLast = TRUE)), ]
writeOGR(tempshp, paste0(envrmt$path_data_validation, "tempshp.shp"),
         "tempshp", driver="ESRI Shapefile", overwrite_layer = TRUE)

ohne3 <- raster::merge(tifs[[1]], tifs[[2]], tifs[[4]], tifs[[5]], tifs[[6]])

ohne3stat <- ft(ohne2, mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")

x <-0
stats <- list()
for (i in seq(0.05,0.1, 0.005)){
  x <- x+1
  stats[[x]] <- ft(raster = ohne3, mul = i, sum = 0.5, minHeight = 8, 
                   points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")
  print(x)
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

ohne3test <- ft(tifs[[3]], mul = 0.08, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation_valshp, "valpoints_3.shp"), lay = "valpoints_3")

ohne3test <- ft(tifs[[3]], mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation_valshp, "valpoints_3.shp"), lay = "valpoints_3")

####ohne4####
tempshp <- raster::intersect(pts, tifs[[4]])
tempshp <- rbind(pts, tempshp)
tempshp <- tempshp[!(duplicated(tempshp@coords) | duplicated(tempshp@coords, fromLast = TRUE)), ]
writeOGR(tempshp, paste0(envrmt$path_data_validation, "tempshp.shp"),
         "tempshp", driver="ESRI Shapefile", overwrite_layer = TRUE)

ohne4 <- raster::merge(tifs[[1]], tifs[[2]], tifs[[3]], tifs[[5]], tifs[[6]])

ohne4stat <- ft(ohne4, mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")

x <-0
stats <- list()
for (i in seq(0.075,0.085, 0.001)){
  x <- x+1
  stats[[x]] <- ft(raster = ohne4, mul = i, sum = 0.5, minHeight = 8, 
                   points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")
  print(x)
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

ohne4test <- ft(tifs[[4]], mul = 0.08, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation_valshp, "Val_Sarah.shp"), lay = "Val_Sarah")

ohne4test <- ft(tifs[[4]], mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation_valshp, "Val_Sarah.shp"), lay = "Val_Sarah")

####ohne5####
tempshp <- raster::intersect(pts, tifs[[5]])
tempshp <- rbind(pts, tempshp)
tempshp <- tempshp[!(duplicated(tempshp@coords) | duplicated(tempshp@coords, fromLast = TRUE)), ]
writeOGR(tempshp, paste0(envrmt$path_data_validation, "tempshp.shp"),
         "tempshp", driver="ESRI Shapefile", overwrite_layer = TRUE)

ohne5 <- raster::merge(tifs[[1]], tifs[[2]], tifs[[3]], tifs[[4]], tifs[[6]])

ohne5stat <- ft(ohne5, mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")

x <-0
stats <- list()
for (i in seq(0.075,0.087, 0.001)){
  x <- x+1
  stats[[x]] <- ft(raster = ohne5, mul = i, sum = 0.5, minHeight = 8, 
                   points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")
  print(x)
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

ohne5test <- ft(tifs[[5]], mul = 0.084, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation_valshp, "val_tobi.shp"), lay = "val_tobi")

ohne5test <- ft(tifs[[5]], mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation_valshp, "val_tobi.shp"), lay = "val_tobi")


####ohne6####
tempshp <- raster::intersect(pts, tifs[[6]])
tempshp <- rbind(pts, tempshp)
tempshp <- tempshp[!(duplicated(tempshp@coords) | duplicated(tempshp@coords, fromLast = TRUE)), ]
writeOGR(tempshp, paste0(envrmt$path_data_validation, "tempshp.shp"),
         "tempshp", driver="ESRI Shapefile", overwrite_layer = TRUE)

ohne6 <- raster::merge(tifs[[1]], tifs[[2]], tifs[[3]], tifs[[4]], tifs[[5]])

ohne6stat <- ft(ohne6, mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")

x <-0
stats <- list()
for (i in seq(0.075,0.085, 0.001)){
  x <- x+1
  stats[[x]] <- ft(raster = ohne6, mul = i, sum = 0.5, minHeight = 8, 
                   points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")
  print(x)
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

ohne6test <- ft(tifs[[6]], mul = 0.081, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation_valshp, "valpoints_6.shp"), lay = "valpoints_6")

ohne6test <- ft(tifs[[6]], mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation_valshp, "valpoints_6.shp"), lay = "valpoints_6")




alletifs <- raster::merge(tifs[[1]], tifs[[2]], tifs[[3]], tifs[[4]], tifs[[5]], tifs[[6]])

gesamt <- ft(alletifs, mul = 0.0775, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation_valshp, "val_all.shp"), lay = "val_all")

gesamt07 <- ft(alletifs, mul = 0.07, sum = 0.5, minHeight = 8, 
             points = paste0(envrmt$path_data_validation_valshp, "val_all.shp"), lay = "val_all")

gesamt85 <- ft(alletifs, mul = 0.085017, sum = 0.5, minHeight = 8, 
               points = paste0(envrmt$path_data_validation_valshp, "val_all.shp"), lay = "val_all")


tempshp <- raster::intersect(pts, tifs[[1]])
tempshp <- rbind(pts, tempshp)
tempshp <- tempshp[!(duplicated(tempshp@coords) | duplicated(tempshp@coords, fromLast = TRUE)), ]
writeOGR(tempshp, paste0(envrmt$path_data_validation, "tempshp.shp"),
         "tempshp", driver="ESRI Shapefile", overwrite_layer = TRUE)

ohne1 <- raster::merge(tifs[[2]], tifs[[3]], tifs[[4]], tifs[[5]], tifs[[6]])

ohne1stat <- ft(ohne1, mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")

x <-0
stats <- list()
for (i in seq(0.095,0.10, 0.001)){
  x <- x+1
  stats[[x]] <- ft(raster = ohne1, mul = i, sum = 0.5, minHeight = 8, 
                   points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")
  print(x)
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

ohne1test <- ft(tifs[[1]], mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation_valshp, "Val_Frauke.shp"), lay = "Val_Frauke")

####ohne1_second_val####
tempshp <- raster::intersect(pts, tifs[[1]])
tempshp <- rbind(pts, tempshp)
tempshp <- tempshp[!(duplicated(tempshp@coords) | duplicated(tempshp@coords, fromLast = TRUE)), ]
writeOGR(tempshp, paste0(envrmt$path_data_validation, "tempshp.shp"),
         "tempshp", driver="ESRI Shapefile", overwrite_layer = TRUE)

ohne1 <- raster::merge(tifs[[2]], tifs[[3]], tifs[[4]], tifs[[5]])

ohne1stat <- ft(ohne1, mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")

x <-0
stats <- list()
for (i in seq(0.08,0.11, 0.002)){
  x <- x+1
  stats[[x]] <- ft(raster = ohne1, mul = i, sum = 0.5, minHeight = 8, 
                   points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")
  print(x)
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

tempshp <- raster::intersect(pts, tifs[[1]])
writeOGR(tempshp, paste0(envrmt$path_data_validation, "tempshp.shp"),
         "tempshp", driver="ESRI Shapefile", overwrite_layer = TRUE)
ohne1test <- ft(tifs[[1]], mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")

###ohne2 second val####
tempshp <- raster::intersect(pts, tifs[[2]])
tempshp <- rbind(pts, tempshp)
tempshp <- tempshp[!(duplicated(tempshp@coords) | duplicated(tempshp@coords, fromLast = TRUE)), ]
writeOGR(tempshp, paste0(envrmt$path_data_validation, "tempshp.shp"),
         "tempshp", driver="ESRI Shapefile", overwrite_layer = TRUE)

ohne2 <- raster::merge(tifs[[1]], tifs[[3]], tifs[[4]], tifs[[5]])

ohne2stat <- ft(ohne2, mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")

x <-0
stats <- list()
for (i in seq(0.03,0.11, 0.005)){
  x <- x+1
  stats[[x]] <- ft(raster = ohne1, mul = i, sum = 0.5, minHeight = 8, 
                   points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")
  print(x)
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

tempshp <- raster::intersect(pts, tifs[[2]])
writeOGR(tempshp, paste0(envrmt$path_data_validation, "tempshp.shp"),
         "tempshp", driver="ESRI Shapefile", overwrite_layer = TRUE)
ohne2test <- ft(tifs[[2]], mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")

####ohne3 val 2####
tempshp <- raster::intersect(pts, tifs[[3]])
tempshp <- rbind(pts, tempshp)
tempshp <- tempshp[!(duplicated(tempshp@coords) | duplicated(tempshp@coords, fromLast = TRUE)), ]
writeOGR(tempshp, paste0(envrmt$path_data_validation, "tempshp.shp"),
         "tempshp", driver="ESRI Shapefile", overwrite_layer = TRUE)

ohne3 <- raster::merge(tifs[[1]], tifs[[2]], tifs[[4]], tifs[[5]])

ohne3stat <- ft(ohne2, mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")

x <-0
stats <- list()
for (i in seq(0.03,0.11, 0.005)){
  x <- x+1
  stats[[x]] <- ft(raster = ohne1, mul = i, sum = 0.5, minHeight = 8, 
                   points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")
  print(x)
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

tempshp <- raster::intersect(pts, tifs[[3]])
writeOGR(tempshp, paste0(envrmt$path_data_validation, "tempshp.shp"),
         "tempshp", driver="ESRI Shapefile", overwrite_layer = TRUE)
ohne3test <- ft(tifs[[3]], mul = 0.07, sum = 0.5, minHeight = 8, 
                points = paste0(envrmt$path_data_validation, "tempshp.shp"), lay = "tempshp")
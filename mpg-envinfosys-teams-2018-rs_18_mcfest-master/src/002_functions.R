#####indices function ####
#@https://github.com/environmentalinformatics-marburg/satelliteTools/blob/master/R/rgbIndices.R

rgbIndices<- function(rgb,
                      rgbi=c("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI", "ExGR", "VEG")) {
  
  ## compatibility check
  if (raster::nlayers(rgb) < 3)
    stop("Argument 'rgb' needs to be a Raster* object with at least 3 layers (usually red, green and blue).")
  
  ### processing
  
  
  ## separate visible bands
  red <- rgb[[1]]
  green <- rgb[[2]]
  blue <- rgb[[3]]
  
  indices <- lapply(rgbi, function(item){
    ## calculate Visible Vegetation Index vvi
    if (item=="VVI"){
      cat("\ncalculate Visible Vegetation Index (VVI)")
      VVI <- (1 - abs((red - 30) / (red + 30))) * 
        (1 - abs((green - 50) / (green + 50))) * 
        (1 - abs((blue - 1) / (blue + 1)))
      names(VVI) <- "VVI"
      return(VVI)
      
    } else if (item=="VARI"){
      # calculate Visible Atmospherically Resistant Index (VARI)
      cat("\ncalculate Visible Atmospherically Resistant Index (VARI)")
      VARI<-(green-red)/(green+red-blue)
      names(VARI) <- "VARI"
      return(VARI)
      
    } else if (item=="NDTI"){
      ## Normalized difference turbidity index
      cat("\ncalculate Normalized difference turbidity index (NDTI)")
      NDTI<-(red-green)/(red+green)
      names(NDTI) <- "NDTI"
      return(NDTI)
      
    } else if (item=="RI"){
      # redness index
      cat("\ncalculate redness index (RI)")
      RI<-red**2/(blue*green**3)
      names(RI) <- "RI"
      return(RI)
      
    } else if (item=="CI"){
      # CI Soil Colour Index
      cat("\ncalculate Soil Colour Index (CI)")
      CI<-(red-green)/(red+green)
      names(CI) <- "CI"
      return(CI)
      
    } else if (item=="BI"){
      #  Brightness Index
      cat("\ncalculate Brightness Index (BI)")
      BI<-sqrt((red**2+green**2+blue*2)/3)
      names(BI) <- "BI"
      return(BI)
      
    } else if (item=="SI"){
      # SI Spectra Slope Saturation Index
      cat("\ncalculate Spectra Slope Saturation Index (SI)")
      SI<-(red-blue)/(red+blue) 
      names(SI) <- "SI"
      return(SI)
      
    } else if (item=="HI"){    
      # HI Primary colours Hue Index
      cat("\ncalculate Primary colours Hue Index (HI)")
      HI<-(2*red-green-blue)/(green-blue)
      names(HI) <- "HI"
      return(HI)
      
    } else if (item=="TGI"){
      # Triangular greenness index
      cat("\ncalculate Triangular greenness index (TGI)")
      TGI <- -0.5*(190*(red - green)- 120*(red - blue))
      names(TGI) <- "TGI"
      return(TGI)
      
    } else if (item=="GLI"){
      cat("\ncalculate Green leaf index (GLI)")
      # Green leaf index
      GLI<-(2*green-red-blue)/(2*green+red+blue)
      names(GLI) <- "GLI"
      return(GLI)
      
    } else if (item=="NGRDI"){
      # NGRDI Normalized green red difference index 
      cat("\ncalculate Normalized green red difference index  (NGRDI)")
      NGRDI<-(green-red)/(green+red) 
      names(NGRDI) <- "NGRDI"
      return(NGRDI)
      
    } else if (item=="ExG"){
      # NGRDI Normalized green red difference index 
      cat("\ncalculate Excess Green Index (ExG)")
      ExG <- 2*green - red - blue 
      names(ExG) <- "ExG"
      return(ExG)
    
    } else if (item=="ExGR"){
      # NGRDI Normalized green red difference index 
      cat("\ncalculate  Excess Green Index - Excess Red Index (ExGR)")
      ExGR<- (2*green - red - blue) - (1.4*red - green)
      names(ExGR) <- "ExGR"
      return(ExGR)
      
    } else if (item=="VEG"){
      # NGRDI Normalized green red difference index 
      cat("\ncalculate  Vegetative Index (VEG)")
      VEG<- green / (red**0.667 * blue**0.333)
      names(VEG) <- "VEG"
      return(VEG)
      
    } else if (item=="CIVE"){
      # NGRDI Normalized green red difference index 
      cat("\ncalculate Color Index of Vegetation Extraction (CIVE)")
      CIVE<- 0.441*red - 0.881*green + 0.385*blue + 18.78745
      names(CIVE) <- "CIVE"
      return(CIVE)
      
    } else if (item=="COM"){
      # NGRDI Normalized green red difference index 
      cat("\ncalculate Combined Index (COM)")
      COM<- 0.25*(2*green - red - blue) + 0.3* ((2*green - red - blue) - (1.4*red - green)) + 0.33* (0.441*red - 0.881*green + 0.385*blue + 18.78745) + 0.12* (green / (red**0.667 * blue**0.333))
      names(COM) <- "COM"
      return(COM)
      
    } else if (item=="CEV"){
      # NGRDI Normalized green red difference index 
      cat("\ncalculate Combination aus Ponti (CEV)")
      CEV<- 0.33*((1 - abs((red - 30) / (red + 30))) * (1 - abs((green - 50) / (green + 50))) * (1 - abs((blue - 1) / (blue + 1)))) + 0.33*(2*green - red - blue) + 0.33*(0.441*red - 0.881*green + 0.385*blue + 18.78745)
      names(CEV) <- "CEV"
      return(CEV)
    
    } else if (item=="CEV_gew"){
      # NGRDI Normalized green red difference index 
      cat("\ncalculate Combination aus Ponti (CEV_gew)")
      CEV_gew<- 0.4*((1 - abs((red - 30) / (red + 30))) * (1 - abs((green - 50) / (green + 50))) * (1 - abs((blue - 1) / (blue + 1)))) + 0.2*(2*green - red - blue) + 0.4*(0.441*red - 0.881*green + 0.385*blue + 18.78745)
      names(CEV_gew) <- "CEV_gew"
      return(CEV_gew)
    
    } else if (item=="CEV_ohne"){
      # NGRDI Normalized green red difference index 
      cat("\ncalculate Combination aus Ponti (CEV_ohne)")
      CEV_ohne<- ((1 - abs((red - 30) / (red + 30))) * (1 - abs((green - 50) / (green + 50))) * (1 - abs((blue - 1) / (blue + 1)))) + (2*green - red - blue) + (0.441*red - 0.881*green + 0.385*blue + 18.78745)
      names(CEV_ohne) <- "CEV_ohne"
      return(CEV_ohne)
      
    } else if (item=="mcfesti"){
      # NGRDI Normalized green red difference index 
      cat("\ncalculate mcfesti (mcfesti)")
      mcfesti <- 0.25*(sqrt((red**2+green**2+blue*2)/3)) + 0.25*(0.441*red - 0.881*green + 0.385*blue + 18.78745) + 0.25*((1 - abs((red - 30) / (red + 30))) * (1 - abs((green - 50) / (green + 50))) * (1 - abs((blue - 1) / (blue + 1)))) + 0.25*(-0.5*(190*(red - green)- 120*(red - blue)))
      names(mcfesti) <- "mcfesti"
      return(mcfesti)
    }
    
      else if (item=="test"){
      # NGRDI Normalized green red difference index 
      cat("\ncalculate mcfesti (test)")
      test <- ((red - 2*green)/(red + 2*green)) + blue
      names(test) <- "test"
      return(test)
  }
  })
  return(raster::stack(indices))
}

#####filter scratch####

filter <- function(rs, fil=c("mean5", "mean15", "mean21", "mean31", "sobel5", "sobel15", "sobel21", "sobel31", 
                                "gauss5", "gauss15", "gauss21", "gauss31", "LoG5", "LoG15", "LoG21", "LoG31")){

  prog <- lapply(fil, function(t){
    
    if (t=="mean5"){
      #mean 5 pix 2,5m
      cat("\ncalc mean5")
      mean5 <- raster::focal(rs, matrix(1/25, nrow = 5, ncol = 5), fun = sum)
      names(mean5) <- "mean5"
      return(mean5)
    }
    else if (t=="mean15"){
      #mean 15 pix 7,5m
      cat("\ncalc mean15")
      mean15 <- raster::focal(rs, matrix(1/225, nrow = 15, ncol = 15), fun = sum)
      names(mean15) <- "mean15"
      return(mean15)
    }
    else if (t=="mean21"){
      #mean 21 pix 10,5m
      cat("\ncalc mean21")
      mean21 <- raster::focal(rs, matrix(1/441, nrow = 21, ncol = 21), fun = sum)
      names(mean21) <- "mean21"
      return(mean21)
    }
    else if (t=="mean31"){
      #mean 31 pix 15,5m
      cat("\ncalc mean31")
      mean31 <- raster::focal(rs, matrix(1/961, nrow = 31, ncol = 31), fun = sum)
      names(mean31) <- "mean31"
      return(mean31)
    }
    else if (t=="sobel5"){
      #sobel 5 pix 2,5m
      cat("\ncalc sobel5")
      sobel5 <- sqrt(raster::focal(rs, matrix(c(2,1,0,-1,-2,2,1,0,-2,-1,4,2,0,-2,-4,2,1,0,-1,-2,2,1,0,-1,-2),nrow=5), fun = sum)**2+
                       raster::focal(rs, matrix(c(-2,-2,-4,-2,-2,-1,-1,-2,-1,-1,0,0,0,0,0,1,1,2,1,1,2,2,4,2,2),nrow=5), fun = sum)**2)
      names(sobel5) <- "sobel5"
      return(sobel5)
    }
    else if (t=="sobel15"){
      #sobel 15 pix 7,5m
      cat("\ncalc sobel5")
      sobel15 <- sqrt(raster::focal(rs, matrix(c(rep(-64, 7), -128, rep(-64, 7), rep(-32, 7), -64, rep(-32, 7), rep(-16, 7), -32, rep(-16, 7), rep(-8, 7), -16, rep(-8, 7), rep(-4, 7), -8, rep(-4, 7), rep(-2, 7), -4, rep(-2, 7), rep(-1, 7), -2, rep(-1, 7),
                                                      rep(0, 15),
                                                      rep(1, 7), 2, rep(1, 7), rep(2, 7), 4, rep(2, 7), rep(4, 7), 8, rep(4, 7), rep(8, 7), 16, rep(8, 7), rep(16, 7), 32, rep(16, 7), rep(32, 7), 64, rep(32, 7), rep(64, 7), 128, rep(64, 7)), nrow = 15), fun = sum)**2 +
                        raster::focal(rs, t(matrix(c(rep(-64, 7), -128, rep(-64, 7), rep(-32, 7), -64, rep(-32, 7), rep(-16, 7), -32, rep(-16, 7), rep(-8, 7), -16, rep(-8, 7), rep(-4, 7), -8, rep(-4, 7), rep(-2, 7), -4, rep(-2, 7), rep(-1, 7), -2, rep(-1, 7),
                                                          rep(0, 15),
                                                          rep(1, 7), 2, rep(1, 7), rep(2, 7), 4, rep(2, 7), rep(4, 7), 8, rep(4, 7), rep(8, 7), 16, rep(8, 7), rep(16, 7), 32, rep(16, 7), rep(32, 7), 64, rep(32, 7), rep(64, 7), 128, rep(64, 7)), nrow = 15)), fun = sum)**2)
      names(sobel15) <- "sobel15"
      return(sobel15)
    }
    else if (t=="sobel21"){
      #sobel 21 pix 10,5m
      cat("\ncalc sobe21")
      sobel21 <- sqrt(raster::focal(rs, matrix(c(rep(-512, 10), -1024, rep(-512, 10), rep(-256, 10), -512, rep(-256, 10), rep(-128, 10), -256, rep(-128, 10), rep(-64, 10), -128, rep(-64, 10), rep(-32, 10), -64, rep(-32, 10), rep(-16, 10), -32, rep(-16, 10), rep(-8, 10), -16, rep(-8, 10), rep(-4, 10), -8, rep(-4, 10), rep(-2, 10), -4, rep(-2, 10), rep(-1, 10), -2, rep(-1, 10),
                                                      rep(0, 21),
                                                      rep(1, 10), 2, rep(1, 10), rep(2, 10), 4, rep(2, 10), rep(4, 10), 8, rep(4, 10), rep(8, 10), 16, rep(8, 10), rep(16, 10), 32, rep(16, 10), rep(32, 10), 64, rep(32, 10), rep(64, 10), 128, rep(64, 10), rep(128, 10), 256, rep(128, 10), rep(256, 10), 512, rep(256, 10), rep(512, 10), 1024, rep(512, 10)), nrow = 21), fun = sum)**2 +
                        raster::focal(rs, t(matrix(c(rep(-512, 10), -1024, rep(-512, 10), rep(-256, 10), -512, rep(-256, 10), rep(-128, 10), -256, rep(-128, 10), rep(-64, 10), -128, rep(-64, 10), rep(-32, 10), -64, rep(-32, 10), rep(-16, 10), -32, rep(-16, 10), rep(-8, 10), -16, rep(-8, 10), rep(-4, 10), -8, rep(-4, 10), rep(-2, 10), -4, rep(-2, 10), rep(-1, 10), -2, rep(-1, 10),
                                                          rep(0, 21),
                                                          rep(1, 10), 2, rep(1, 10), rep(2, 10), 4, rep(2, 10), rep(4, 10), 8, rep(4, 10), rep(8, 10), 16, rep(8, 10), rep(16, 10), 32, rep(16, 10), rep(32, 10), 64, rep(32, 10), rep(64, 10), 128, rep(64, 10), rep(128, 10), 256, rep(128, 10), rep(256, 10), 512, rep(256, 10), rep(512, 10), 1024, rep(512, 10)), nrow = 21)), fun = sum)**2)
      names(sobel21) <- "sobel21"
      return(sobel21)
    }
    else if (t=="sobel31"){
      #sobel 31 pix 15,5m
      cat("\ncalc sobe31")
      sobel31 <- sqrt(raster::focal(rs, matrix(c(rep(-16384, 15), -32768, rep(-16384, 15), rep(-8192, 15), -16384, rep(-8192, 15), rep(-4096, 15), -8192, rep(-4096, 15), rep(-2048, 15), -4096, rep(-2048, 15), rep(-1524, 15), -2048, rep(-1524, 15), rep(-512, 15), -1524, rep(-512, 15), rep(-256, 15), -512, rep(-256, 15), rep(-128, 15), -256, rep(-128, 15), rep(-64, 15), -128, rep(-64, 15), rep(-32, 15), -64, rep(-32, 15), rep(-16, 15), -32, rep(-16, 15), rep(-8, 15), -16, rep(-8, 15), rep(-4, 15), -8, rep(-4, 15), rep(-2, 15), -4, rep(-2, 15), rep(-1, 15), -2, rep(-1, 15),
                                                      rep(0, 31),
                                                      rep(1, 15), 2, rep(1, 15), rep(2, 15), 4, rep(2, 15), rep(4, 15), 8, rep(4, 15), rep(8, 15), 16, rep(8, 15), rep(16, 15), 32, rep(16, 15), rep(32, 15), 64, rep(32, 15), rep(64, 15), 128, rep(64, 15), rep(128, 15), 256, rep(128, 15), rep(256, 15), 512, rep(256, 15), rep(512, 15), 1524, rep(512, 15), rep(1524, 15), 2048, rep(1524, 15), rep(2048, 15), 4096, rep(2048, 15), rep(4096, 15), 8192, rep(4096, 15), rep(8192, 15), 16384, rep(8192, 15), rep(16384, 15), 32768, rep(16384, 15)), nrow = 31), fun = sum)**2 +
                        raster::focal(rs, t(matrix(c(rep(-16384, 15), -32768, rep(-16384, 15), rep(-8192, 15), -16384, rep(-8192, 15), rep(-4096, 15), -8192, rep(-4096, 15), rep(-2048, 15), -4096, rep(-2048, 15), rep(-1524, 15), -2048, rep(-1524, 15), rep(-512, 15), -1524, rep(-512, 15), rep(-256, 15), -512, rep(-256, 15), rep(-128, 15), -256, rep(-128, 15), rep(-64, 15), -128, rep(-64, 15), rep(-32, 15), -64, rep(-32, 15), rep(-16, 15), -32, rep(-16, 15), rep(-8, 15), -16, rep(-8, 15), rep(-4, 15), -8, rep(-4, 15), rep(-2, 15), -4, rep(-2, 15), rep(-1, 15), -2, rep(-1, 15),
                                                          rep(0, 31),
                                                          rep(1, 15), 2, rep(1, 15), rep(2, 15), 4, rep(2, 15), rep(4, 15), 8, rep(4, 15), rep(8, 15), 16, rep(8, 15), rep(16, 15), 32, rep(16, 15), rep(32, 15), 64, rep(32, 15), rep(64, 15), 128, rep(64, 15), rep(128, 15), 256, rep(128, 15), rep(256, 15), 512, rep(256, 15), rep(512, 15), 1524, rep(512, 15), rep(1524, 15), 2048, rep(1524, 15), rep(2048, 15), 4096, rep(2048, 15), rep(4096, 15), 8192, rep(4096, 15), rep(8192, 15), 16384, rep(8192, 15), rep(16384, 15), 32768, rep(16384, 15)), nrow = 31)), fun = sum)**2)
      names(sobel31) <- "sobel31"
      return(sobel31)
    }
    else if (t=="gauss5"){
      #gauss 5 pix 2,5m
      cat("\ncalc gauss5")
      gauss5 <- raster::focal(rs,  matrix(c(1,1,2,1,1,1,2,4,2,1,2,4,8,4,2,1,2,4,2,1,1,1,2,1,1),nrow=5), fun = sum)
      names(gauss5) <- "gauss5"
      return(gauss5)
    }
    else if (t=="gauss15"){
      #gauss 15 pix 7,5m
      cat("\ncalc gauss15")
      gauss15 <- raster::focal(rs, w=smoothie::kernel2dmeitsjer(type = "gauss",nx=15,ny=15,sigma=1),fun = sum)
      names(gauss15) <- "gauss15"
      return(gauss15)
    }
    else if (t=="gauss21"){
      #gauss 21 pix 10,5m
      cat("\ncalc gauss21")
      gauss21 <- raster::focal(rs, w=smoothie::kernel2dmeitsjer(type = "gauss",nx=21,ny=21,sigma=1),fun = sum)
      names(gauss21) <- "gauss21"
      return(gauss21)
    }
    else if (t=="gauss31"){
      #gauss 31 pix 15,5m
      cat("\ncalc gauss21")
      gauss31 <- raster::focal(rs, w=smoothie::kernel2dmeitsjer(type = "gauss",nx=31,ny=31,sigma=1),fun = sum)
      names(gauss31) <- "gauss31"
      return(gauss31)
    }
    else if (t=="LoG5"){
      #laplacian of gaussian 5 pix 2,5m
      cat("\ncalc LoG5")
      LoG5 <- raster::focal(rs, w=smoothie::kernel2dmeitsjer(type = "LoG", nx=5,ny=5,sigma=1),fun = sum)
      names(LoG5) <- "LoG5"
      return(LoG5)
    }
    else if (t=="LoG15"){
      #laplacian of gaussian 15 pix 7,5m
      cat("\ncalc LoG15")
      LoG15 <- raster::focal(rs, w=smoothie::kernel2dmeitsjer(type = "LoG", nx=15,ny=15,sigma=1),fun = sum)
      names(LoG15) <- "LoG15"
      return(LoG15)
    }
    else if (t=="LoG21"){
      #laplacian of gaussian 21 pix 10,5m
      cat("\ncalc LoG21")
      LoG21 <- raster::focal(rs, w=smoothie::kernel2dmeitsjer(type = "LoG", nx=21,ny=21,sigma=1),fun = sum)
      names(LoG21) <- "LoG21"
      return(LoG21)
    }
    else if (t=="LoG31"){
      #laplacian of gaussian 31 pix 15,5m
      cat("\ncalc LoG31")
      LoG31 <- raster::focal(rs, w=smoothie::kernel2dmeitsjer(type = "LoG", nx=31,ny=31,sigma=1),fun = sum)
      names(LoG31) <- "LoG31"
      return(LoG31)
    }
    })
  return(raster::stack(prog))
}

# #glcm1 5 pix 2,5m
# glcm5 <- glcm::glcm(rs ,n_grey, window = c(5,5))
# #glcm1 15 pix 7,5m
# glcm15 <- glcm::glcm(rs ,n_grey, window = c(15,15))
# #glcm1 21 pix 10,5m
# glcm21 <- glcm::glcm(rs ,n_grey, window = c(21,21))
# #glcm1 31 pix 15,5m
# glcm31 <- glcm::glcm(rs ,n_grey, window = c(31,31))

#glcm2 5 pix 2,5m
#glcm2 15 pix 7,5m
#glcm2 21 pix 10,5m
#glcm2 31 pix 15,5m

#haralick 5 pix 2,5m
#haralick 15 pix 7,5m
#haralick 21 pix 10,5m
#haralick 31 pix 15,5m

#####function segement test####
ft <- function(raster, mul, sum, minHeight, points, lay){
  
  pos <- ForestTools::vwf(raster, winFun = function(x){x * mul + sum}, 
                          minHeight = minHeight, minWinNeib = "queen", verbose = TRUE, maxWinDiameter = 30)
  crow <- uavRst::chmseg_FT(treepos = pos, chm = raster, minTreeAlt = minHeight, format = "polygons", verbose = TRUE)
  
  pts <- rgdal::readOGR(dsn = points, layer = lay)
  pts <- spTransform(pts,crs(crow))
  
  pwp <- ForestTools::sp_summarise(trees = pts, areas = crow)
  crow@data$TreeCount <- pwp@data$TreeCount
  crow@data$TreeCount [is.na(crow@data$TreeCount)] <- 0
  
  writeOGR(crow, paste0(envrmt$path_data_validation, "crow.shp"),
           "crow", driver="ESRI Shapefile", overwrite_layer = TRUE)
  #crow <- readOGR(dsn = "F:/09_Semester/mpg-envinsys-plygrnd/data/lidar/segtest/nadel_laub_test/crow.shp", layer = "crow")
  
  #General
  total_points <- length(pts@data$id)
  total_poly <- length(crow@data$layer)
  
  #Poly stats
  pb <- length(crow@data$TreeCount[crow@data$TreeCount == 1]) # polygons with == one tree
  pkb <- length(crow@data$TreeCount[crow@data$TreeCount < 1]) #empty polygons - polygon without a tree
  mbp <- length(crow@data$TreeCount[crow@data$TreeCount > 1]) #polygons with more than one tree
  hit_ratio <- (pb+mbp) / length(crow@data$layer) #polygons with one tree plus polys with multiple trees on all validation points
  acc <- pb / length(crow@data$layer)
  balance <- hit_ratio - acc
  error_ratio <- pkb / length(crow@data$layer) #ratio of empty polygons to all created polygons
  dev <- (abs(total_poly-total_points))/(total_points) #percentage of deviation of polys to points
  
  
  #Tree stats
  twop <- sum(crow@data$TreeCount[crow@data$TreeCount > 1])- mbp
  top <- sum(crow@data$TreeCount) - twop
  
  #percentage of trees without own polygon
  twup <- (sum(crow@data$TreeCount[crow@data$TreeCount > 1])- mbp) / (sum(crow@data$TreeCount [crow@data$TreeCount == 1]) + sum(crow@data$TreeCount [crow@data$TreeCount > 1]))
  #percentage trees with own polygon
  thup <- 1 - twup
  
  tre_tot_bal <- sum(crow@data$TreeCount [crow@data$TreeCount > 1])
  
  metrics <- data.frame(mul, sum)
  poly <- data.frame(pb, pkb, mbp, hit_ratio, total_poly, acc, balance, error_ratio, dev)
  tree <- data.frame(twop, top, twup, thup, total_points, tre_tot_bal)
  perf <- list(metrics, poly, tree)
  cat("\n")
  cat("\n")
  return(perf)
}


#####Pix extraction#####
extraction <- function(shp, rasfiles){
  ext_tab <- lapply(seq(length(shp)), function(i){
    ext_ind <- lapply(seq(length(rasfiles)), function(x){
      stack <- raster::stack(rasfiles[x])
      ext <- raster::extract(stack, shp[[i]], na.rm = FALSE, df=TRUE)
      cat("\n", rasfiles[x],
          "\nShape:", shp[[i]]@data$type[1], 
          "\nTif", x, "of 54 \n")
      print(Sys.time())
      return(ext)
    })
    tab <- do.call("cbind", ext_ind)
    for (t in unique(tab$ID)){
      tab$abt [tab$ID == t] <- shp[[i]]@data$abt[shp[[i]]@data$id == t]
    }
    #names(tab) <- paste0(names(tab), "_", shp[[i]]@data$type[1])
    tab <- tab[!duplicated(as.list(names(tab)))]
    tab$type <- shp[[i]]@data$type[1]
    return(tab)
  })
}

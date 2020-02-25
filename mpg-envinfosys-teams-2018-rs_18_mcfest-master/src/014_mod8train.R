root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

traindat_all_2 <- read.csv(file = paste0(envrmt$path_data_training, "traindat_all_2.csv"), header = TRUE, sep = ";", dec = ".")
traindat_all_2 <- traindat_all_2[!traindat_all_2$type == "ESH",]
traindat_all_2$type <- droplevels(traindat_all_2$type)
traindat_all_2$runnum <- 1:nrow(traindat_all_2)
bu  <- traindat_all_2$runnum[traindat_all_2$ID %in% c(33, 14, 25, 42, 49, 28, 8, 56, 2) & traindat_all_2$type == "BU"]	
tei  <- traindat_all_2$runnum[traindat_all_2$ID %in% c(14, 26, 2, 10, 6, 21, 17) & traindat_all_2$type == "TEI"]	
x <- c(bu, tei)	
traindat_all_2 <- traindat_all_2[!traindat_all_2$runnum %in% x,]	
traindat_all_2$tyid <- apply( traindat_all_2[ , 1:2 ] , 1 , paste0 , collapse = "")

set.seed(1899)
smp <- caret::createDataPartition(y = traindat_all_2$tyid, p = .80, list = FALSE)
pred <- traindat_all_2[-smp,]	
res <- traindat_all_2[-smp,]
ind = CAST::CreateSpacetimeFolds(pred, spacevar = "tyid", k = 5)
pred <- pred[, 18:51]	
res <- res[, 2]	

trainctl <- caret::trainControl(method = "cv", number = 5, classProbs = TRUE, 	
                                savePredictions = TRUE, returnResamp = "all", index = ind$index, indexOut = ind$indexOut)


autoStopCluster <- function(cl) {	
  stopifnot(inherits(cl, "cluster"))	
  env <- new.env()	
  env$cluster <- cl	
  attr(cl, "gcMe") <- env	
  reg.finalizer(env, function(e) {	
    message("Finalizing cluster ...")	
    message(capture.output(print(e$cluster)))	
    try(parallel::stopCluster(e$cluster), silent = FALSE)	
    message("Finalizing cluster ... done")	
  })	
  cl	
}	

cl <- autoStopCluster(parallel::makeCluster(11))
doParallel::registerDoParallel(cl)
mod8 <- caret::train(predictors = pred, response = res, method = "rf", importance = TRUE, 
                     trControl = trainctl, metric = "Kappa")
saveRDS(mod8, "J:/mod8.rds")
print(Sys.time())	
gc()


mod8 <- readRDS(paste0(envrmt$path_data_training, "mod8.rds"))
test8 <- predict(mod8, traindat_all_2[smp,])
conf8 <- caret::confusionMatrix(test8, traindat_all_2[smp, 2])

rasfiles <- list.files(paste0(envrmt$path_data_aerial_processed_selection), pattern = "*.tif", full.names = TRUE)	
sta <- stack(rasfiles[15:48])
areapred8 <- raster::predict(sta, mod8)
writeRaster(areapred8, paste0(envrmt$path_data_training, "areapredmod8.tif"), overwrite = TRUE)

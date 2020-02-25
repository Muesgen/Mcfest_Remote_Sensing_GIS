root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",	
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")	

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))	

traindat_all_2 <- read.csv(file = paste0(envrmt$path_data_training, "traindat_all_2.csv"), header = TRUE, sep = ";", dec = ".")	
traindat_all_2 <- traindat_all_2[!traindat_all_2$type == "ESH",]	
traindat_all_2$type <- droplevels(traindat_all_2$type)	
traindat_all_2$runnum  <- 1:nrow(traindat_all_2)	

bu  <- traindat_all_2$runnum[traindat_all_2$ID %in% c(33, 14, 25, 42, 49, 28, 8, 56, 2) & traindat_all_2$type == "BU"]	
tei  <- traindat_all_2$runnum[traindat_all_2$ID %in% c(14, 26, 2, 10, 6, 21, 17) & traindat_all_2$type == "TEI"]	
x <- c(bu, tei)	
traindat_all_2 <- traindat_all_2[!traindat_all_2$runnum %in% x,]	
traindat_all_2$tyid <- apply( traindat_all_2[ , 1:2 ] , 1 , paste0 , collapse = "")	

set.seed(1899)	
smp <- caret::createDataPartition(y = traindat_all_2$tyid, p = .80, list = FALSE)	
pred <- traindat_all_2[-smp,]	
res <- traindat_all_2[-smp,]	
#ind = CAST::CreateSpacetimeFolds(pred, spacevar = "abt", k = 5)	
pred <- pred[, c(4:8, 10:54)]	
res <- res[, 2]	

trainctl <- caret::trainControl(method = "cv", number = 5, classProbs = TRUE, 	
                                savePredictions = TRUE, returnResamp = "all")	

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

cl <- autoStopCluster(parallel::makeCluster(8))	
doParallel::registerDoParallel(cl)	
mod6 <- CAST::ffs(predictors = pred, response = res, method = "rf", importance = TRUE, trControl = trainctl, 	
                  metric = "Kappa")	
saveRDS(mod6, "J:/mod6.rds")	
print(Sys.time())	
gc()	

mod6 <- readRDS(paste0(envrmt$path_data_training, "mod6.rds"))	

test <- predict(mod6, traindat_all_2[smp,-9])	
conf <- caret::confusionMatrix(test, traindat_all_2$type[smp])
saveRDS(conf, paste0(envrmt$path_data_training, "confmod6.rds"))

rasfiles <- list.files(paste0(envrmt$path_data_aerial_processed_selection), pattern = "*.tif", full.names = TRUE)	
sta <- stack(rasfiles)	

areapred <- raster::predict(sta[[-6]], mod6)	
writeRaster(areapred, paste0(envrmt$path_data_training, "areapredmod6.tif"), overwrite = TRUE)
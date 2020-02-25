root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

traindat_all_2 <- read.csv(file = paste0(envrmt$path_data_training, "traindat_all_2.csv"), header = TRUE, sep = ";", dec = ".")
traindat_all_2 <- traindat_all_2[!traindat_all_2$type == "ESH",]
traindat_all_2$type <- droplevels(traindat_all_2$type)

set.seed(1899)
smp <- caret::createDataPartition(y = traindat_all_2$type, p = .80, list = FALSE)
pred <- traindat_all_2[-smp,]
res <- traindat_all_2[-smp,]
ind = CAST::CreateSpacetimeFolds(pred, spacevar = "abt", k = 5)
pred <- pred[, c(4:8, 10:54)]
res <- res[, 2]

trainctl <- caret::trainControl(method = "cv", number = 5, classProbs = TRUE, 
                                index = ind$index, indexOut = ind$indexOut, savePredictions = TRUE, returnResamp = "all")

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
mod3 <- CAST::ffs(predictors = pred, response = res, method = "rf", importance = TRUE, trControl = trainctl, 
                  metric = "Kappa")
saveRDS(mod3, "J:/mod3.rds")
print(Sys.time())
gc()

mod3 <- readRDS(paste0(envrmt$path_data_training, "mod3.rds"))

test <- predict(mod4, traindat_all_2[smp,-9])
conf <- caret::confusionMatrix(test, traindat_all_2$type[smp])

rasfiles <- list.files(paste0(envrmt$path_data_aerial_processed_selection), pattern = "*.tif", full.names = TRUE)
sta <- stack(rasfiles)

areapred <- raster::predict(sta[[-6]], mod3)
writeRaster(areapred, paste0(envrmt$path_data_training, "areapredmod3.tif"), overwrite = TRUE)
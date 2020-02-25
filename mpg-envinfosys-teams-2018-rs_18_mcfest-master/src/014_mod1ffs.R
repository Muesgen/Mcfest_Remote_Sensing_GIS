root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

traindat <- read.csv(file = paste0(envrmt$path_data_training, "traindat.csv"), header = TRUE, sep = ";", dec = ".")

##Model Unbrauchbar, da kein Seed
smp <- caret::createDataPartition(y = traindat$type, p = .80, list = FALSE)
pred <- traindat[-smp,]
res <- traindat[-smp,]
ind = CAST::CreateSpacetimeFolds(pred, spacevar = "abt", k = 5)
pred <- pred[, c(4:54)]
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
mod1 <- CAST::ffs(predictors = pred, response = res, method = "rf", importance = TRUE, trControl = trainctl, 
                  metric = "Accuracy")
saveRDS(mod1, "J:/mod1.rds")
print(Sys.time())
gc()

mod1 <- readRDS(paste0(envrmt$path_data_training, "mod1.rds"))

test <- predict(mod1, traindat[smp,])
conf <- caret::confusionMatrix(test, traindat$type[smp])

rasfiles <- list.files(paste0(envrmt$path_data_aerial_processed_selection), pattern = "*.tif", full.names = TRUE)
sta <- stack(rasfiles)

areapred <- raster::predict(sta, mod1)
writeRaster(areapred, paste0(envrmt$path_data_training, "areapred.tif"), overwrite = TRUE)
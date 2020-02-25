root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

traindat_all_2 <- read.csv(file = paste0(envrmt$path_data_training, "traindat_all_2.csv"), header = TRUE, sep = ";", dec = ".")
traindat_all_2 <- traindat_all_2[!traindat_all_2$type == "ESH",]
traindat_all_2$type <- droplevels(traindat_all_2$type)

bu  <- traindat_all_2$runnum[traindat_all_2$ID %in% c(33, 14, 25, 42, 49, 28, 8, 56, 2) & traindat_all_2$type == "BU"]
tei  <- traindat_all_2$runnum[traindat_all_2$ID %in% c(14, 26, 2, 10, 6, 21, 17) & traindat_all_2$type == "TEI"]
x <- c(bu, tei)
traindat_all_2 <- traindat_all_2[!traindat_all_2$runnum %in% x,]

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
mod4 <- CAST::ffs(predictors = pred, response = res, method = "rf", importance = TRUE, trControl = trainctl, 
                  metric = "Kappa")
saveRDS(mod4, "J:/mod4.rds")
print(Sys.time())
gc()

mod4 <- readRDS(paste0(envrmt$path_data_training, "mod4.rds"))

test <- predict(mod4, traindat_all_2[smp,-9])
conf <- caret::confusionMatrix(test, traindat_all_2$type[smp])

rasfiles <- list.files(paste0(envrmt$path_data_aerial_processed_selection), pattern = "*.tif", full.names = TRUE)
sta <- stack(rasfiles)

areapred <- raster::predict(sta[[-6]], mod4)
writeRaster(areapred, paste0(envrmt$path_data_training, "areapredmod4.tif"), overwrite = TRUE)

#Attempt to split pred dataset to 20k each instead of 60k all at once
predtraindat <- traindat_all_2[smp,-9]
set.seed(1899)
sample(seq(nrow(traindat_all_2[smp, -9])), 20000)
x <- sample(1:4,size=nrow(traindat_all_2[smp,-9]),replace=TRUE,prob=c(0.80,0.10,0.08, 0.02))
pred1 <- predtraindat[x==1,]
pred2 <- predtraindat[x==2,]
pred3 <- predtraindat[x==3,]
pred4 <- predtraindat[x==4,]

pred1p <- predict(mod4, pred1)
conf1 <- caret::confusionMatrix(pred1p, pred1$type)

pred2p <- predict(mod4, pred2)
conf2 <- caret::confusionMatrix(pred2p, pred2$type)

pred3p <- predict(mod4, pred3)
conf3 <- caret::confusionMatrix(pred3p, pred3$type)

pred4p <- predict(mod4, pred4)
conf4 <- caret::confusionMatrix(pred4p, pred4$type)
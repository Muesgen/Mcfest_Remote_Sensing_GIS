root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

traindat_all_2 <- read.csv(file = paste0(envrmt$path_data_training, "traindat_all_2.csv"), header = TRUE, sep = ";", dec = ".")
traindat_all_2 <- traindat_all_2[!traindat_all_2$type == "ESH",]
traindat_all_2$type <- droplevels(traindat_all_2$type)
traindat_all_2$runnum <- 1:nrow(traindat_all_2)

set.seed(1899)
bu <- traindat_all_2[traindat_all_2$type == "BU",]
smp <- sample(nrow(bu), 1000)
bu <- bu[smp,]

dgl <- traindat_all_2[traindat_all_2$type == "DGL",]
smp <- sample(nrow(dgl), 1000)
dgl <- dgl[smp,]

fi <- traindat_all_2[traindat_all_2$type == "FI",]
smp <- sample(nrow(fi), 1000)
fi <- fi[smp,]

lar <- traindat_all_2[traindat_all_2$type == "LAR",]
smp <- sample(nrow(lar), 1000)
lar <- lar[smp,]

tei <- traindat_all_2[traindat_all_2$type == "TEI",]
smp <- sample(nrow(tei), 1000)
tei <- tei[smp,]

traindat_1000 <- rbind(bu, dgl, fi, lar, tei)
# write.table(traindat_1000, file = paste0(envrmt$path_data_training, "traindat_1000_size_mod7.csv"), row.names = FALSE, 
#             dec = ".", sep = ";")
set.seed(1899)
pred <- traindat_1000[, 4:54]
res <- traindat_1000[, 2]

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
mod7 <- CAST::ffs(predictors = pred, response = res, method = "rf", importance = TRUE, trControl = trainctl, 
                  metric = "Kappa")
saveRDS(mod2, "J:/mod7.rds")
print(Sys.time())
gc()

mod7 <- readRDS(paste0(envrmt$path_data_training, "mod7.rds"))
caret::varImp(mod7)


traindat_1000 <- read.csv(paste0(envrmt$path_data_training, "traindat_1000_size_mod7.csv"), sep = ";")
traindat_test <- traindat_all_2[-traindat_1000$runnum,]

test <- predict(mod7, traindat_test)
conf <- caret::confusionMatrix(test, traindat_test$type)
saveRDS(conf, paste0(envrmt$path_data_training, "confmod7.rds"))

areapred <- raster::predict(sta[[-6]], mod7)
writeRaster(areapred, paste0(envrmt$path_data_training, "areapredmod7.tif"), overwrite = TRUE)
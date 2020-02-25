root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

traindat_all_2 <- read.csv(file = paste0(envrmt$path_data_training, "traindat_all_2.csv"), header = TRUE, sep = ";", dec = ".")
traindat_all_2 <- traindat_all_2[!traindat_all_2$type == "ESH",]
traindat_all_2$type <- droplevels(traindat_all_2$type)
traindat_all_2$runnum <- 1:nrow(traindat_all_2)
traindat_all_2$tyid <- apply( traindat_all_2[ , 1:2 ] , 1 , paste0 , collapse = "")


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
set.seed(1899)
ind = CAST::CreateSpacetimeFolds(traindat_1000, spacevar = "tyid", k = 5)
pred <- traindat_1000[, 4:54]
res <- traindat_1000[, 2]

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
mod9 <- caret::train(predictors = pred, response = res, method = "rf", importance = TRUE, 
                     trControl = trainctl, metric = "Kappa")
saveRDS(mod9, "J:/mod9.rds")
print(Sys.time())	
gc()

mod9 <- readRDS(paste0(envrmt$path_data_training, "mod9.rds"))
traindat_1000 <- read.csv(paste0(envrmt$path_data_training, "traindat_1000_size.csv"), sep = ";")
traindat_test <- traindat_all_2[-traindat_1000$runnum,]

test9 <- predict(mod9, traindat_test)
conf9 <- caret::confusionMatrix(test9, traindat_test$type)

rasfiles <- list.files(paste0(envrmt$path_data_aerial_processed_selection), pattern = "*.tif", full.names = TRUE)	
sta <- stack(rasfiles[15:48])
areapred9 <- raster::predict(sta, mod9)
writeRaster(areapred9, paste0(envrmt$path_data_training, "areapredmod9.tif"), overwrite = TRUE)

root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

traindat_all_2 <- read.csv(file = paste0(envrmt$path_data_training, "traindat_all_2.csv"), 
                           header = TRUE, sep = ";", dec = ".")

#####2K aus jeder Baumart ziehen####
set.seed(332211)
bu <- traindat_all_2[traindat_all_2$type == "BU",]
smp <- sample(nrow(bu), 1000)
bu <- bu[smp,]

dgl <- traindat_all_2[traindat_all_2$type == "DGL",]
smp <- sample(nrow(dgl), 1000)
dgl <- dgl[smp,]

esh <- traindat_all_2[traindat_all_2$type == "ESH",]
smp <- sample(nrow(esh), 1000)
esh <- esh[smp,]

fi <- traindat_all_2[traindat_all_2$type == "FI",]
smp <- sample(nrow(fi), 1000)
fi <- fi[smp,]

lar <- traindat_all_2[traindat_all_2$type == "LAR",]
smp <- sample(nrow(lar), 1000)
lar <- lar[smp,]

tei <- traindat_all_2[traindat_all_2$type == "TEI",]
smp <- sample(nrow(tei), 1000)
tei <- tei[smp,]

traindat_1000 <- rbind(bu, dgl, esh, fi, lar, tei)
write.table(traindat, file = paste0(envrmt$path_data_training, "traindat_1000_size.csv"), row.names = FALSE, 
            dec = ".", sep = ";")
set.seed(332211)
ind = CAST::CreateSpacetimeFolds(traindat, spacevar = "abt", k = 5)
pred <- traindat[, 4:54]
res <- traindat[, 2]

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
mod2 <- CAST::ffs(predictors = pred, response = res, method = "rf", importance = TRUE, trControl = trainctl, 
                  metric = "Kappa")
saveRDS(mod2, "J:/mod2.rds")
print(Sys.time())
gc()

#####Predicting mod2#####
traindat_1000 <- read.csv(paste0(envrmt$path_data_training, "traindat_1000_size.csv"), sep = ";")
traindat_test <- traindat_all_2[-traindat_1000$runnum,]

test <- predict(mod2, traindat_test)
conf <- caret::confusionMatrix(test, traindat_test$type)

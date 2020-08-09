train <- readRDS("/Shared/ssrivastva/mult_ml/data/ml_full.rds")

set.seed(12345)

subIdx <- list()
for (cc in 1:10) {
    subIdx[[cc]] <- sample(1:nrow(train$x), 1e5)
}

saveRDS(subIdx, "/Shared/ssrivastva/mult_ml/data/sub_idx.rds")

set.seed(12345)
for (cc in 1:10) {
    subx <- train$x[subIdx[[cc]], ]
    suby <- train$y[subIdx[[cc]]]
    partIdx <- sample(1:50, nrow(subx), replace = TRUE)
    partData <- list()
    for (kk in 1:50) {
        idx <- which(partIdx == kk)
        partData[[kk]] <- list(x = subx[idx, ], y = suby[idx])
    }
    fname <- paste0("/Shared/ssrivastva/mult_ml/data/part_rep_", cc, ".rds")
    saveRDS(partData, fname)
}

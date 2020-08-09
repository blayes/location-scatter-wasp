cmdArgs <- commandArgs(trailingOnly = TRUE)

mtd <- as.numeric(cmdArgs[1])
id <- as.numeric(cmdArgs[2])

if (mtd == 1) {
    library(rstan)
    train <- readRDS("/Shared/ssrivastva/mult_ml/data/ml_full.rds")
    subIdx <- readRDS("/Shared/ssrivastva/mult_ml/data/sub_idx.rds")

    y0 <- as.numeric(train$y)[subIdx[[id]]]
    x0 <- as.matrix(train$x)[subIdx[[id]], ]

    fileName <- "./ml.stan"
    stanCode <- readChar(fileName, file.info(fileName)$size)

    dat <- list(K = 5,
                N = length(y0),
                D = ncol(x0),
                y = y0,
                x = x0
                )

    rtime <- proc.time()
    resStan <- stan(model_code = stanCode, data = dat,
                    chains = 1, iter = 10000, warmup = 5000, thin = 5)

    res <- do.call(cbind, resStan@sim$samples[[1]][1:(ncol(x0) * 4)])
    rtime <- proc.time() - rtime

    fname <- paste0("/Shared/ssrivastva/mult_ml/result/full/full_rep_", id, ".rds")
    saveRDS(list(res = res, time = rtime[3]), fname)
} else {
    print("peace")
}

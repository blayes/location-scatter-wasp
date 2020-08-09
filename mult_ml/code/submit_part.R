cmdArgs <- commandArgs(trailingOnly = TRUE)

mtd <- as.numeric(cmdArgs[1])
id <- as.numeric(cmdArgs[2])

if (mtd == 1) {
    library(rstan)

    npart <- 50
    cvs <- 1:10
    wid <- cbind(rep(1:length(cvs), each = npart), rep(1:npart, times = length(cvs)))

    cid <- wid[id, 1]
    sid <- wid[id, 2]

    cvtrain <- readRDS(paste0("/Shared/ssrivastva/mult_ml/data/part_rep_", cid, ".rds"))
    train <- cvtrain[[sid]]
    rm(cvtrain)

    y0 <- as.numeric(train$y)
    x0 <- as.matrix(train$x)
    nrep0 <- 1e5 / nrow(x0)

    fileName <- "./stoc_ml.stan"
    stanCode <- readChar(fileName, file.info(fileName)$size)

    dat <- list(K = 5,
                N = length(y0),
                D = ncol(x0),
                y = y0,
                x = x0,
                nrep = nrep0
                )

    rtime <- proc.time()
    resStan <- stan(model_code = stanCode, data = dat,
                    chains = 1, iter = 10000, warmup = 5000, thin = 5)
    res <- do.call(cbind, resStan@sim$samples[[1]][1:(ncol(x0) * 4)])
    rtime <- proc.time() - rtime

    fname <- paste0("/Shared/ssrivastva/mult_ml/result/subs/res_rep_", cid, "_sub_", sid, ".rds")
    saveRDS(list(res = res, time = rtime[3]), fname)

} else if (mtd == 2) {
    source("combine.R")

    npart <- 50
    cvs <- 1:10

    cid <- cvs[id]

    stime <- proc.time()
    res <- list(); rtime <- numeric(npart)
    for (sid in 1:npart) {
        fname <- paste0("/Shared/ssrivastva/mult_ml/result/subs/res_rep_", cid, "_sub_", sid, ".rds")
        tmpf <- readRDS(fname)
        res[[sid]] <- tmpf$res
        rtime[sid] <- tmpf$time
    }
    bary <- list()
    for (ii in 1:4) {
        dimIdx <- seq(ii, 24, by = 4)
        bary[[ii]] <- sampleBetas(lapply(res, function(x) x[ , dimIdx]))
    }
    names(bary) <- paste0("beta_cat_", 1:4)
    etime <- proc.time()

    rname <- paste0("/Shared/ssrivastva/mult_ml/result/subs/comb_rep_", cid, ".rds")
    saveRDS(list(res = bary, time = mean(rtime) + etime[3] - stime[3]), rname)

} else {
    print("peace")
}

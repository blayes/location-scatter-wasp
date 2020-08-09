cmdArgs <- commandArgs(trailingOnly = TRUE)

mtd <- as.numeric(cmdArgs[1])
id <- as.numeric(cmdArgs[2])

if (mtd == 1) {
    library(rstan)
    cvs <- rep(1:10, each = 2)
    ndims <- rep(1:2, times = 10)
    cid <- cvs[id]
    did <- ndims[id]

    cvtrain <- readRDS("/Shared/ssrivastva/negbin/data/full_n_1e5.rds")
    train <- cvtrain[[cid]][[did]]
    rm(cvtrain)

    y0 <- as.numeric(train$y)
    x0 <- as.matrix(train$x)

    fileName <- "./nb_reg.stan"
    stanCode <- readChar(fileName, file.info(fileName)$size)

    dat <- list(n = nrow(x0),
                p = ncol(x0),
                x = x0,
                y = y0)

    rtime <- proc.time()
    resStan <- stan(model_code = stanCode, data = dat,
                    chains = 1, iter = 10000, warmup = 5000, thin = 5)

    res <- do.call(cbind, resStan@sim$samples[[1]][1:(ncol(x0) + 1)])
    rtime <- proc.time() - rtime

    fname <- paste0("/Shared/ssrivastva/negbin/result/full/rep_", cid, "_ndim_",  did, "_n_1e5.rds")
    saveRDS(list(res = res, time = rtime[3]), fname)

} else if (mtd == 2) {
    library(rstan)
    cvs <- rep(1:10, each = 2)
    ndims <- rep(1:2, times = 10)
    cid <- cvs[id]
    did <- ndims[id]

    cvtrain <- readRDS("/Shared/ssrivastva/negbin/data/full_n_1e4.rds")
    train <- cvtrain[[cid]][[did]]
    rm(cvtrain)

    y0 <- as.numeric(train$y)
    x0 <- as.matrix(train$x)

    fileName <- "./nb_reg.stan"
    stanCode <- readChar(fileName, file.info(fileName)$size)

    dat <- list(n = nrow(x0),
                p = ncol(x0),
                x = x0,
                y = y0)

    rtime <- proc.time()
    resStan <- stan(model_code = stanCode, data = dat,
                    chains = 1, iter = 10000, warmup = 5000, thin = 5)


    res <- do.call(cbind, resStan@sim$samples[[1]][1:(ncol(x0) + 1)])
    rtime <- proc.time() - rtime

    fname <- paste0("/Shared/ssrivastva/negbin/result/full/rep_", cid, "_ndim_",  did, "_n_1e4.rds")
    saveRDS(list(res = res, time = rtime[3]), fname)

} else if (mtd == 3) {
    library(rstan)
    npart <- 20
    cvs <- rep(1:10, each = 2)
    ndims <- rep(1:2, times = 10)
    tmp <- cbind(cvs, ndims)
    wid <- cbind(tmp[rep(1:nrow(tmp), each = npart), ], rep(1:npart, times = 20))

    cid <- wid[id, 1]
    did <- wid[id, 2]
    sid <- wid[id, 3]

    cvtrain <- readRDS(paste0("/Shared/ssrivastva/negbin/data/part_cv_", cid, "_k_", npart, "_n_1e5_m_5000.rds"))
    train <- cvtrain[[did]][[sid]]
    rm(cvtrain)

    y0 <- as.numeric(train$y)
    x0 <- as.matrix(train$x)
    nrep0 <- 1e5 / nrow(x0)

    fileName <- "./stoc_nb_reg.stan"
    stanCode <- readChar(fileName, file.info(fileName)$size)

    dat <- list(n = nrow(x0),
                p = ncol(x0),
                nrep = nrep0,
                x = x0,
                y = y0)

    rtime <- proc.time()
    resStan <- stan(model_code = stanCode, data = dat,
                    chains = 1, iter = 10000, warmup = 5000, thin = 5)

    res <- do.call(cbind, resStan@sim$samples[[1]][1:(ncol(x0) + 1)])
    rtime <- proc.time() - rtime

    fname <- paste0("/Shared/ssrivastva/negbin/result/subs/rep_", cid, "_ndim_",  did, "_nsub_", sid, "_k_", npart, "_n_1e5.rds")
    saveRDS(list(res = res, time = rtime[3]), fname)

} else if (mtd == 4) {
    library(rstan)
    npart <- 50
    cvs <- rep(1:10, each = 2)
    ndims <- rep(1:2, times = 10)
    tmp <- cbind(cvs, ndims)
    wid <- cbind(tmp[rep(1:nrow(tmp), each = npart), ], rep(1:npart, times = 20))

    cid <- wid[id, 1]
    did <- wid[id, 2]
    sid <- wid[id, 3]

    cvtrain <- readRDS(paste0("/Shared/ssrivastva/negbin/data/part_cv_", cid, "_k_", npart, "_n_1e5_m_2000.rds"))
    train <- cvtrain[[did]][[sid]]
    rm(cvtrain)

    y0 <- as.numeric(train$y)
    x0 <- as.matrix(train$x)
    nrep0 <- 1e5 / nrow(x0)

    fileName <- "./stoc_nb_reg.stan"
    stanCode <- readChar(fileName, file.info(fileName)$size)

    dat <- list(n = nrow(x0),
                p = ncol(x0),
                x = x0,
                nrep = nrep0,
                y = y0)

    rtime <- proc.time()
    resStan <- stan(model_code = stanCode, data = dat,
                    chains = 1, iter = 10000, warmup = 5000, thin = 5)

    res <- do.call(cbind, resStan@sim$samples[[1]][1:(ncol(x0) + 1)])
    rtime <- proc.time() - rtime

    fname <- paste0("/Shared/ssrivastva/negbin/result/subs/rep_", cid, "_ndim_",  did, "_nsub_", sid, "_k_", npart, "_n_1e5.rds")
    saveRDS(list(res = res, time = rtime[3]), fname)

} else if (mtd == 5) {
    library(rstan)
    npart <- 20
    cvs <- rep(1:10, each = 2)
    ndims <- rep(1:2, times = 10)
    tmp <- cbind(cvs, ndims)
    wid <- cbind(tmp[rep(1:nrow(tmp), each = npart), ], rep(1:npart, times = 20))

    cid <- wid[id, 1]
    did <- wid[id, 2]
    sid <- wid[id, 3]

    cvtrain <- readRDS(paste0("/Shared/ssrivastva/negbin/data/part_cv_", cid, "_k_", npart, "_n_1e4_m_500.rds"))
    train <- cvtrain[[did]][[sid]]
    rm(cvtrain)

    y0 <- as.numeric(train$y)
    x0 <- as.matrix(train$x)
    nrep0 <- 1e4 / nrow(x0)

    fileName <- "./stoc_nb_reg.stan"
    stanCode <- readChar(fileName, file.info(fileName)$size)

    dat <- list(n = nrow(x0),
                p = ncol(x0),
                nrep = nrep0,
                x = x0,
                y = y0)

    rtime <- proc.time()
    resStan <- stan(model_code = stanCode, data = dat,
                    chains = 1, iter = 10000, warmup = 5000, thin = 5)

    res <- do.call(cbind, resStan@sim$samples[[1]][1:(ncol(x0) + 1)])
    rtime <- proc.time() - rtime

    fname <- paste0("/Shared/ssrivastva/negbin/result/subs/rep_", cid, "_ndim_",  did, "_nsub_", sid, "_k_", npart, "_n_1e4.rds")
    saveRDS(list(res = res, time = rtime[3]), fname)

} else if (mtd == 6) {
    library(rstan)
    npart <- 50
    cvs <- rep(1:10, each = 2)
    ndims <- rep(1:2, times = 10)
    tmp <- cbind(cvs, ndims)
    wid <- cbind(tmp[rep(1:nrow(tmp), each = npart), ], rep(1:npart, times = 20))

    cid <- wid[id, 1]
    did <- wid[id, 2]
    sid <- wid[id, 3]

    cvtrain <- readRDS(paste0("/Shared/ssrivastva/negbin/data/part_cv_", cid, "_k_", npart, "_n_1e4_m_200.rds"))
    train <- cvtrain[[did]][[sid]]
    rm(cvtrain)

    y0 <- as.numeric(train$y)
    x0 <- as.matrix(train$x)
    nrep0 <- 1e4 / nrow(x0)

    fileName <- "./stoc_nb_reg.stan"
    stanCode <- readChar(fileName, file.info(fileName)$size)

    dat <- list(n = nrow(x0),
                p = ncol(x0),
                nrep = nrep0,
                x = x0,
                y = y0)

    rtime <- proc.time()
    resStan <- stan(model_code = stanCode, data = dat,
                    chains = 1, iter = 10000, warmup = 5000, thin = 5)

    res <- do.call(cbind, resStan@sim$samples[[1]][1:(ncol(x0) + 1)])
    rtime <- proc.time() - rtime

    fname <- paste0("/Shared/ssrivastva/negbin/result/subs/rep_", cid, "_ndim_",  did, "_nsub_", sid, "_k_", npart, "_n_1e4.rds")
    saveRDS(list(res = res, time = rtime[3]), fname)

} else if (mtd == 7) {
    source("combine.R")

    npart <- 20
    cvs <- rep(1:10, each = 2)
    ndims <- rep(1:2, times = 10)
    tmp <- cbind(cvs, ndims)

    cid <- tmp[id, 1]
    did <- tmp[id, 2]

    stime <- proc.time()
    res <- list()
    cnt <- 0
    for (sid in 1:npart) {
        fname <- paste0("/Shared/ssrivastva/negbin/result/subs/rep_", cid, "_ndim_",  did, "_nsub_", sid, "_k_", npart,"_n_1e5.rds")
        if (file.exists(fname)) {
            cnt <- cnt + 1
            tmpf <- readRDS(fname)$res
            res[[cnt]] <- tmpf[-(1:1000), ]
        }
    }
    bary <- sampleBetas(res)
    etime <- proc.time()

    rname <- paste0("/Shared/ssrivastva/negbin/result/subs/comb_rep_", cid, "_ndim_",  did, "_k_", npart,"_n_1e5.rds")
    saveRDS(list(res = bary, time = etime[3] - stime[3]), rname)

    stime <- proc.time()
    res <- list()
    cnt <- 0
    for (sid in 1:npart) {
        fname <- paste0("/Shared/ssrivastva/negbin/result/subs/rep_", cid, "_ndim_",  did, "_nsub_", sid, "_k_", npart,"_n_1e4.rds")
        if (file.exists(fname)) {
            cnt <- cnt + 1
            tmpf <- readRDS(fname)$res
            res[[cnt]] <- tmpf[-(1:1000), ]
        }
    }
    bary <- sampleBetas(res)
    etime <- proc.time()

    rname <- paste0("/Shared/ssrivastva/negbin/result/subs/comb_rep_", cid, "_ndim_",  did, "_k_", npart,"_n_1e4.rds")
    saveRDS(list(res = bary, time = etime[3] - stime[3]), rname)
} else if (mtd == 8) {
    source("combine.R")

    npart <- 50
    cvs <- rep(1:10, each = 2)
    ndims <- rep(1:2, times = 10)
    tmp <- cbind(cvs, ndims)

    cid <- tmp[id, 1]
    did <- tmp[id, 2]

    stime <- proc.time()
    res <- list()
    cnt <- 0
    for (sid in 1:npart) {
        fname <- paste0("/Shared/ssrivastva/negbin/result/subs/rep_", cid, "_ndim_",  did, "_nsub_", sid, "_k_", npart,"_n_1e5.rds")
        if (file.exists(fname)) {
            cnt <- cnt + 1
            tmpf <- readRDS(fname)$res
            res[[cnt]] <- tmpf[-(1:1000), ]
        }
    }
    bary <- sampleBetas(res)
    etime <- proc.time()

    rname <- paste0("/Shared/ssrivastva/negbin/result/subs/comb_rep_", cid, "_ndim_",  did, "_k_", npart,"_n_1e5.rds")
    saveRDS(list(res = bary, time = etime[3] - stime[3]), rname)

    stime <- proc.time()
    res <- list()
    cnt <- 0
    for (sid in 1:npart) {
        fname <- paste0("/Shared/ssrivastva/negbin/result/subs/rep_", cid, "_ndim_",  did, "_nsub_", sid, "_k_", npart,"_n_1e4.rds")
        if (file.exists(fname)) {
            cnt <- cnt + 1
            tmpf <- readRDS(fname)$res
            res[[cnt]] <- tmpf[-(1:1000), ]
        }
    }
    bary <- sampleBetas(res)
    etime <- proc.time()

    rname <- paste0("/Shared/ssrivastva/negbin/result/subs/comb_rep_", cid, "_ndim_",  did, "_k_", npart,"_n_1e4.rds")
    saveRDS(list(res = bary, time = etime[3] - stime[3]), rname)
} else {
    print("peace")
}

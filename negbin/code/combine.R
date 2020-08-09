computeBarycenter <- function (meanList, covList) {
    library(matrixStats)
    library(expm)
    library(MASS)

    ncomp <- length(covList)
    ndim <- nrow(covList[[1]])
    wts <- rep(1, ncomp) / ncomp

    baryMean <- rowMeans(do.call(cbind, meanList))
    baryCov <- diag(1.0, ndim)
    barySd <- sqrtm(baryCov)
    err <- baryCov
    cnt <- 1
    while ((norm(err, type = "F") > 1e-6) & cnt < 500) {
        if (cnt %% 10 == 0)  cat("iter: ", cnt, "\n")

        ssj <- matrix(0.0, nrow = ndim, ncol = ndim)
        for (ii in 1:ncomp) {
            ssj <- ssj + sqrtm(baryCov %*% covList[[ii]])
        }

        tmp <- solve(sqrtm(baryCov))
        baryCovNew <- tmp %*% tcrossprod(ssj / ncomp) %*% tmp
        err <- baryCov - baryCovNew
        baryCov <- baryCovNew
        barySd <- sqrtm(baryCov)
        cnt <- cnt + 1
    }

    list(mean = baryMean, cov = baryCov, sqrt = barySd, iter = cnt)
}

sampleBetas <- function (betaList) {
    library(matrixStats)
    library(expm)

    npart <- length(betaList)
    meanBetas <- list()
    covBetas <- list()

    meanBetas <- lapply(betaList, function(x) colMeans(x))
    covBetas <- lapply(betaList, function(x) cov(x))

    resBary <- computeBarycenter(meanBetas, covBetas)
    muBetas <- resBary$mean
    sigBetas <- resBary$cov
    sqrtSigBetas <- resBary$sqrt

    wtime1 <- proc.time()
    baryList <- list()
    for (ii in 1:npart) {
        tmp <- as(chol2inv(chol(covBetas[[ii]])), "symmetricMatrix")
        tmp1 <- matrix(meanBetas[[ii]], nrow = nrow(betaList[[ii]]), ncol = ncol(betaList[[ii]]), byrow = TRUE)
        centScaledSamps <- sqrtm(tmp) %*% (t(betaList[[ii]] - tmp1))
        baryList[[ii]] <- t(muBetas + sqrtSigBetas %*% centScaledSamps)
    }
    wtime2 <- proc.time()

    xtime1 <- proc.time()
    xueLiang <- list()
    for (ii in 1:npart) {
        tmp1 <- matrix(meanBetas[[ii]], nrow = nrow(betaList[[ii]]), ncol = ncol(betaList[[ii]]), byrow = TRUE)
        tmp2 <- matrix(muBetas, nrow = nrow(betaList[[ii]]), ncol = ncol(betaList[[ii]]), byrow = TRUE)
        xueLiang[[ii]] <- betaList[[ii]] + tmp2 - tmp1
    }
    xtime2 <- proc.time()


    list(
         wasp = do.call(rbind, baryList),
         xueLiang = do.call(rbind, xueLiang),
         xlTime = xtime2[3] - xtime1[3],
         wTime = wtime2[3] - wtime1[3]
         )
}

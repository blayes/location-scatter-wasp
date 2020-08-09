setwd("~/mult_ml/code/")
rm(list = ls())
library("expm")
library(coda)
library(matrixStats)
library(xtable)

calcAccuracy <- function (fullSamps, approxSamps) {
    library(expm)
    fullMean <- colMeans(fullSamps)
    approxMean <- colMeans(approxSamps)

    fullCov <- cov(fullSamps)
    approxCov <- cov(approxSamps)

    trm1 <- sum((fullMean - approxMean)^2)
    trm2 <- sum(abs(diag(fullCov + approxCov - 2 * (sqrtm(sqrtm(approxCov) %*% fullCov %*% sqrtm(approxCov))))))

    sqrt(trm1 + trm2)
}

acc <- array(NA, dim = c(10, 4, 2),
             dimnames = list(
                 paste0("cv", 1:10),
                 paste0("dim", 1:4),
                 c("wasp", "xl")
             ))

eff <- array(NA, dim = c(10, 4, 2),
             dimnames = list(
                 paste0("cv", 1:10),
                 paste0("dim", 1:4),
                 c("wasp", "xl")
             ))

for (cc in 1:10) {
    fres <- readRDS(paste0("/Shared/ssrivastva/mult_ml/result/full/full_rep_", cc, ".rds"))$res
    pres <- readRDS(paste0("/Shared/ssrivastva/mult_ml/result/subs/comb_rep_", cc, ".rds"))
    ftime <- readRDS(paste0("/Shared/ssrivastva/mult_ml/result/full/full_rep_", cc, ".rds"))$time
    for (dd in 1:4) {
        dimIdx <- seq(dd, 24, by = 4)
        acc[cc, dd, "wasp"] <- calcAccuracy(fres[ , dimIdx], pres$res[[dd]]$wasp)
        acc[cc, dd, "xl"] <- calcAccuracy(fres[ , dimIdx], pres$res[[dd]]$xueLiang)
        eff[cc, dd, "wasp"] <- ftime / (pres$time + pres$res[[dd]]$wTime)
        eff[cc, dd, "xl"] <- ftime / (pres$time + pres$res[[dd]]$xlTime)
    }
}

summ <- cbind(apply(acc, c(2, 3), mean),
              apply(eff, c(2, 3), mean))

xtable(format(round(summ, 4), nsmall = 4))

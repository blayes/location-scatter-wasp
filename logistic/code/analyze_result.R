setwd("~/logistic/code/")
rm(list = ls())
library("expm") ## for the matrix sqrt
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

acc <- array(NA, dim = c(2, 2, 10, 2, 2),
             dimnames = list(paste0("n", 1:2),
                             paste0("k", 1:2),
                             paste0("cv", 1:10),
                             paste0("dim", 1:2),
                             c("wasp", "xl")
                             ))

eff <- array(NA, dim = c(2, 2, 10, 2, 2),
             dimnames = list(paste0("n", 1:2),
                             paste0("k", 1:2),
                             paste0("cv", 1:10),
                             paste0("dim", 1:2),
                             c("wasp", "xl")
                             ))
for (cc in 1:10) {
    for (pp in 1:2) {
        fres <- readRDS(paste0("/Shared/ssrivastva/logistic/result/full/rep_", cc, "_ndim_", pp, "_n_1e5.rds"))
        fbeta <- do.call(cbind, fres$res)
        ftime <- fres$time
        for (kk in 1:2) {
            npart <- c(20, 50)[kk]
            fname <- paste0("/Shared/ssrivastva/logistic/result/subs/comb_rep_", cc, "_ndim_",  pp, "_k_", npart, "_n_1e5.rds")
            pres <- readRDS(fname)
            pt <- rep(NA, npart)
            for (tt in 1:npart) {
                tmp <- readRDS(paste0("/Shared/ssrivastva/logistic/result/subs/rep_", cc, "_ndim_",  pp, "_nsub_", tt, "_k_", npart, "_n_1e5.rds"))
                pt[tt] <- tmp$time
            }
            acc[1, kk, cc, pp, "wasp"] <- calcAccuracy(fbeta, pres$res$wasp)
            acc[1, kk, cc, pp, "xl"] <- calcAccuracy(fbeta, pres$res$xueLiang)
            eff[1, kk, cc, pp, "wasp"] <- ftime / (mean(pt) + pres$res$wTime)
            eff[1, kk, cc, pp, "xl"] <- ftime / (mean(pt) + pres$res$xlTime)
        }
    }
}

for (cc in 1:10) {
    for (pp in 1:2) {
        fres <- readRDS(paste0("/Shared/ssrivastva/logistic/result/full/rep_", cc, "_ndim_", pp, "_n_1e4.rds"))
        fbeta <- do.call(cbind, fres$res)
        ftime <- fres$time
        for (kk in 1:2) {
            npart <- c(20, 50)[kk]
            fname <- paste0("/Shared/ssrivastva/logistic/result/subs/comb_rep_", cc, "_ndim_",  pp, "_k_", npart, "_n_1e4.rds")
            pres <- readRDS(fname)
            pt <- rep(NA, npart)
            for (tt in 1:npart) {
                tmp <- readRDS(paste0("/Shared/ssrivastva/logistic/result/subs/rep_", cc, "_ndim_",  pp, "_nsub_", tt, "_k_", npart, "_n_1e4.rds"))
                pt[tt] <- tmp$time
            }
            acc[2, kk, cc, pp, "wasp"] <- calcAccuracy(fbeta, pres$res$wasp)
            acc[2, kk, cc, pp, "xl"] <- calcAccuracy(fbeta, pres$res$xueLiang)
            eff[2, kk, cc, pp, "wasp"] <- ftime / (mean(pt) + pres$res$wTime)
            eff[2, kk, cc, pp, "xl"] <- ftime / (mean(pt) + pres$res$xlTime)
        }
    }
}

accSumm <- round(apply(acc, c(1, 2, 4, 5), mean), 4)
effSumm <- round(apply(eff, c(1, 2, 4, 5), mean), 4)

xtable(format(
    rbind(c(c(accSumm[2, 1, 1, "wasp"], accSumm[2, 1, 1, "xl"], accSumm[2, 1, 2, "wasp"], accSumm[2, 1, 2, "xl"]),
          c(accSumm[2, 2, 1, "wasp"], accSumm[2, 2, 1, "xl"], accSumm[2, 2, 2, "wasp"], accSumm[2, 2, 2, "xl"])),
          c(c(accSumm[1, 1, 1, "wasp"], accSumm[1, 1, 1, "xl"], accSumm[1, 1, 2, "wasp"], accSumm[1, 1, 2, "xl"]),
          c(accSumm[1, 2, 1, "wasp"], accSumm[1, 2, 1, "xl"], accSumm[1, 2, 2, "wasp"], accSumm[1, 2, 2, "xl"])
          )
          ), nsmall = 4
)
)

xtable(format(
    rbind(c(c(effSumm[2, 1, 1, "wasp"], effSumm[2, 1, 1, "xl"], effSumm[2, 1, 2, "wasp"], effSumm[2, 1, 2, "xl"]),
          c(effSumm[2, 2, 1, "wasp"], effSumm[2, 2, 1, "xl"], effSumm[2, 2, 2, "wasp"], effSumm[2, 2, 2, "xl"])),
          c(c(effSumm[1, 1, 1, "wasp"], effSumm[1, 1, 1, "xl"], effSumm[1, 1, 2, "wasp"], effSumm[1, 1, 2, "xl"]),
          c(effSumm[1, 2, 1, "wasp"], effSumm[1, 2, 1, "xl"], effSumm[1, 2, 2, "wasp"], effSumm[1, 2, 2, "xl"])
          )
          ), nsmall = 4
)
)

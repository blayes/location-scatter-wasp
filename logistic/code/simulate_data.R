rm(list=ls())

set.seed(12345)

setwd("~/logistic/code")
library(matrixStats)
library(Matrix)

genData <- function (nobs, ndim, ntrail) {
    ## fixed effects coefficients
    fixef <- rep(c(-2, 2), length = ndim)

    x <- matrix(rnorm(nobs * ndim), nobs, ndim)

    ## compute linear predictors and generate observations
    mu <- drop(x %*% fixef)
    probs <- 1 / (1 + exp(- mu))
    y <- rbinom(n = nobs, size = ntrail, prob = probs)

    list(x = x, mu = mu, probs = probs, y = y)
}

nobs <- 1e5
ndim <- c(10, 20)
ntrail <- 15

repData <- list()
for (cc in 1:10) {
  repData[[cc]] <- vector("list", 2)
  names(repData[[cc]]) <- c("p10", "p20")
}

for (cc in 1:10) {
  cat("cc ", cc, "\n")
  for (pp in 1:2) {
      repData[[cc]][[pp]] <- genData(nobs, ndim[pp], ntrail)
  }
}

saveRDS(repData, "/Shared/ssrivastva/logistic/data/full_n_1e5.rds")

## subsets: n = 1e5
rm(list = ls())

repData <- readRDS("/Shared/ssrivastva/logistic/data/full_n_1e5.rds")

nobs <- 1e5
ndim <- c(10, 20)
ntrail <- 15

set.seed(12345)

## k = 20, m = 5000
npart <- 20
partData <- list()

for (cc in 1:10) {
    partData <- vector("list", 2)
    names(partData) <- c("p10", "p20")
    for (pp in 1:2) {
        partData[[pp]] <- vector("list", npart)
        names(partData[[pp]]) <- paste0("k", 1:npart)
        lst <- repData[[cc]][[pp]]
        for (ll in 1:npart) {
            idx <- sample(1:nrow(lst$x), 5000)
            partData[[pp]][[ll]]$nobs <- length(idx)
            partData[[pp]][[ll]]$x <- lst$x[idx, ]
            partData[[pp]][[ll]]$y <- lst$y[idx]
            partData[[pp]][[ll]]$idx <- idx
            partData[[pp]][[ll]]$nrep <- nobs / length(idx)
        }
    }
    saveRDS(partData, paste0("/Shared/ssrivastva/logistic/data/part_cv_", cc, "_k_", npart, "_n_1e5_m_5000.rds"))
    cat("cc: ", cc, "\n")
}

set.seed(12345)

## k = 50, m = 2000
npart <- 50
partData <- list()

for (cc in 1:10) {
    partData <- vector("list", 2)
    names(partData) <- c("p10", "p20")
    for (pp in 1:2) {
        partData[[pp]] <- vector("list", npart)
        names(partData[[pp]]) <- paste0("k", 1:npart)
        lst <- repData[[cc]][[pp]]
        for (ll in 1:npart) {
            idx <- sample(1:nrow(lst$x), 2000)
            partData[[pp]][[ll]]$nobs <- length(idx)
            partData[[pp]][[ll]]$x <- lst$x[idx, ]
            partData[[pp]][[ll]]$y <- lst$y[idx]
            partData[[pp]][[ll]]$idx <- idx
            partData[[pp]][[ll]]$nrep <- nobs / length(idx)
        }
    }
    saveRDS(partData, paste0("/Shared/ssrivastva/logistic/data/part_cv_", cc, "_k_", npart, "_n_1e5_m_2000.rds"))
    cat("cc: ", cc, "\n")
}


set.seed(12345)

## k = 100, m = 1000
npart <- 100
partData <- list()

for (cc in 1:10) {
    partData <- vector("list", 2)
    names(partData) <- c("p10", "p20")
    for (pp in 1:2) {
        partData[[pp]] <- vector("list", npart)
        names(partData[[pp]]) <- paste0("k", 1:npart)
        lst <- repData[[cc]][[pp]]
        for (ll in 1:npart) {
            idx <- sample(1:nrow(lst$x), 1000)
            partData[[pp]][[ll]]$nobs <- length(idx)
            partData[[pp]][[ll]]$x <- lst$x[idx, ]
            partData[[pp]][[ll]]$y <- lst$y[idx]
            partData[[pp]][[ll]]$idx <- idx
            partData[[pp]][[ll]]$nrep <- nobs / length(idx)
        }
    }
    saveRDS(partData, paste0("/Shared/ssrivastva/logistic/data/part_cv_", cc, "_k_", npart, "_n_1e5_m_1000.rds"))
    cat("cc: ", cc, "\n")
}

######################################
############### n = 1e4 ##############
######################################
rm(list=ls())

set.seed(12345)

setwd("~/logistic/code")
library(matrixStats)
library(Matrix)

genData <- function (nobs, ndim, ntrail) {
    ## fixed effects coefficients
    fixef <- rep(c(-2, 2), length = ndim)

    x <- matrix(rnorm(nobs * ndim), nobs, ndim)

    ## compute linear predictors and generate observations
    mu <- drop(x %*% fixef)
    probs <- 1 / (1 + exp(- mu))
    y <- rbinom(n = nobs, size = ntrail, prob = probs)

    list(x = x, mu = mu, probs = probs, y = y)
}

nobs <- 1e4
ndim <- c(10, 20)
ntrail <- 15

repData <- list()
for (cc in 1:10) {
  repData[[cc]] <- vector("list", 2)
  names(repData[[cc]]) <- c("p10", "p20")
}

for (cc in 1:10) {
  cat("cc ", cc, "\n")
  for (pp in 1:2) {
      repData[[cc]][[pp]] <- genData(nobs, ndim[pp], ntrail)
  }
}

saveRDS(repData, "/Shared/ssrivastva/logistic/data/full_n_1e4.rds")


## subsets: n = 1e4
rm(list = ls())

repData <- readRDS("/Shared/ssrivastva/logistic/data/full_n_1e4.rds")

nobs <- 1e4
ndim <- c(10, 20)
ntrail <- 15

set.seed(12345)

## k = 20, m = 500
npart <- 20
partData <- list()

for (cc in 1:10) {
    partData <- vector("list", 2)
    names(partData) <- c("p10", "p20")
    for (pp in 1:2) {
        partData[[pp]] <- vector("list", npart)
        names(partData[[pp]]) <- paste0("k", 1:npart)
        lst <- repData[[cc]][[pp]]
        for (ll in 1:npart) {
            idx <- sample(1:nrow(lst$x), 500)
            partData[[pp]][[ll]]$nobs <- length(idx)
            partData[[pp]][[ll]]$x <- lst$x[idx, ]
            partData[[pp]][[ll]]$y <- lst$y[idx]
            partData[[pp]][[ll]]$idx <- idx
            partData[[pp]][[ll]]$nrep <- nobs / length(idx)
        }
    }
    saveRDS(partData, paste0("/Shared/ssrivastva/logistic/data/part_cv_", cc, "_k_", npart, "_n_1e4_m_500.rds"))
    cat("cc: ", cc, "\n")
}

set.seed(12345)

## k = 50, m = 200
npart <- 50
partData <- list()

for (cc in 1:10) {
    partData <- vector("list", 2)
    names(partData) <- c("p10", "p20")
    for (pp in 1:2) {
        partData[[pp]] <- vector("list", npart)
        names(partData[[pp]]) <- paste0("k", 1:npart)
        lst <- repData[[cc]][[pp]]
        for (ll in 1:npart) {
            idx <- sample(1:nrow(lst$x), 200)
            partData[[pp]][[ll]]$nobs <- length(idx)
            partData[[pp]][[ll]]$x <- lst$x[idx, ]
            partData[[pp]][[ll]]$y <- lst$y[idx]
            partData[[pp]][[ll]]$idx <- idx
            partData[[pp]][[ll]]$nrep <- nobs / length(idx)
        }
    }
    saveRDS(partData, paste0("/Shared/ssrivastva/logistic/data/part_cv_", cc, "_k_", npart, "_n_1e4_m_200.rds"))
    cat("cc: ", cc, "\n")
}

set.seed(12345)

## k = 100, m = 100
npart <- 100
partData <- list()

for (cc in 1:10) {
    partData <- vector("list", 2)
    names(partData) <- c("p10", "p20")
    for (pp in 1:2) {
        partData[[pp]] <- vector("list", npart)
        names(partData[[pp]]) <- paste0("k", 1:npart)
        lst <- repData[[cc]][[pp]]
        for (ll in 1:npart) {
            idx <- sample(1:nrow(lst$x), 100)
            partData[[pp]][[ll]]$nobs <- length(idx)
            partData[[pp]][[ll]]$x <- lst$x[idx, ]
            partData[[pp]][[ll]]$y <- lst$y[idx]
            partData[[pp]][[ll]]$idx <- idx
            partData[[pp]][[ll]]$nrep <- nobs / length(idx)
        }
    }
    saveRDS(partData, paste0("/Shared/ssrivastva/logistic/data/part_cv_", cc, "_k_", npart, "_n_1e4_m_100.rds"))
    cat("cc: ", cc, "\n")
}

#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#utility functions for diagnostic
#
###############################################################################
###############################################################################
.prob.pno <- function(mcmclist, prob) {


    diag <- mcmclist$diagnostic$diag

    con <- paste0(100 * prob, "%") %in% names(diag)

    # Select mean from diag ===================================================

    if (con[1] && con[2]) {

        # Get mean from diag ----------------------------------

        diagMean <- diag[, c("Parameter", "Chain", paste0(100 * prob, "%"),
                        "Mean")]

    } else {

        # Compute mean ----------------------------------------

        diag <- .diag.mcmcobj(mcmclist$mcmcobj, mcmclist$information$parts,
                              prob)

        diagMean <- diag[, c("Parameter", "Chain", paste0(100 * prob, "%"),
                        "Mean")]

    }

    # List by parameter -------------------------------------

    diaglist <- split(diagMean, diag$Parameter)

    I <- nrow(diaglist$theta)

    J <- nrow(diaglist$a)

    a <- matrix(diaglist$a$Mean, nrow = 1, byrow = TRUE)

    bm <- matrix(diaglist$b$Mean, nrow = I, ncol = J, byrow = TRUE)

    mij <- vector("list", 3)

    for (i in 1:3) {

        theta <- matrix(diaglist$theta[, c(paste0(100 * prob, "%"),
                                           "Mean")[i]],
                        nrow = I, ncol = 1, byrow = FALSE)

        mij[[i]] <- (theta %*% a) - bm

    }

    # Probability for 2pnob and 3pnob =========================================

    if ("mcmc.2pnob" %in% class(mcmclist)) {

        pij <- lapply(mij, pnorm)

    }


    if ("mcmc.3pnob" %in% class(mcmclist)) {

        cm <- matrix(diaglist$c$Mean, nrow = I, ncol = J, byrow = TRUE)

        pij <- lapply(mij, function(x, ...) {
                           return(cm + (1 - cm) * pnorm(x, ...))
                           })

    }


    names(pij) <- c(paste0(100 * prob, "%"), "Mean")

    return(pij)

}

###############################################################################
###############################################################################

.diag.mcmcobj <- function(mcmcobj, parts, ...) {

    list <- lapply(mcmcobj, function(x) {
                            return(.diag.matrix(x, parts, ...))
                            })

    rf <- sapply(mcmcobj, ncol)

    ri <- c(1, rf)


    for (i in 1:length(mcmcobj)) {

        list[[i]][, 1] <- names(mcmcobj)[i]

        rownames(list[[i]]) <- sum(ri[1:i]):sum(rf[1:i])

    }


    f <- rep(names(mcmcobj), rf)

    diag <- unsplit(list, f)

    return(as.data.frame(diag))

}

###############################################################################
###############################################################################

.diag.matrix <- function(mcmcmatrix, parts, ...) {

    J <- ncol(mcmcmatrix)

    mat <- apply(mcmcmatrix, 2, function(x) {
                                  return(.diag(x, parts, ...))
                                  })

    mat <- as.data.frame(t(mat))

    new <- cbind(rep(NA, J), 1:J)

    matfin <- as.data.frame(cbind.data.frame(new, mat))

    names(matfin) <- c("Parameter", "Chain", names(mat))

    return(matfin)

}

###############################################################################
###############################################################################

.diag <- function(mcmc, parts,
                  probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975), ...) {


    mean <- mean(mcmc, na.rm = TRUE)

    sd <- sd(mcmc, na.rm = TRUE)

    quan <- quantile(mcmc, probs, na.rm = TRUE)

    mcmclist <- .mcmc.divide(mcmc, parts)

    gelman <- coda::gelman.diag(mcmclist, ...)

    diag <- c(unlist(gelman), mean, sd, quan)

    names(diag) <- c("Rhat", "Upper", "Mean", "Sd", names(quan))

    return(diag)

}

###############################################################################
###############################################################################

.mcmc.divide <- function(mcmc, parts) {


    mcmc <- as.matrix(mcmc)

    n <- parts

    I <- nrow(mcmc)

    t <- trunc(I/n)

    li <- rep(NA, n)


    n.chain <- vector("list", n)

    for (i in 1:n) {

        li[i] <- I - (i * t) + 1

    }

    li <- sort(li)

    ls <- c(li[2:n] - 1, I)


    for (j in 1:n) {

        n.chain[[j]] <- coda::as.mcmc(mcmc[li[j]:ls[j], ])

    }

    return(coda::mcmc.list(n.chain))

}


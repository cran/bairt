#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#utility functions for mcmc.3pnob
#
###############################################################################
###############################################################################
.interval <- function(iter, burning, thin) {


    int <- seq(thin, iter, by = thin)

    int <- int[int > burning]

    return(int)

}

###############################################################################
###############################################################################
.iniv.3ponb <- function(c.prior, J, data0) {


    a <- rep(1, J)

    b <- qnorm((colMeans(data0, na.rm = TRUE) + 0.01)/1.02)

    c.p <- matrix(c.prior, nrow = J, ncol = 2, byrow = TRUE)

    c <- c.p[, 1]/rowSums(c.p)

    theta <- qnorm((rowMeans(data0, na.rm = TRUE) + 0.01)/1.02)

    theta <- theta - mean(theta)

    vinicia <- list("a" = a, "b" = b, "c" = c, "theta" = theta)

    return(vinicia)

}

###############################################################################
###############################################################################
.draw.w.3ponb <- function(nij, c, I, J, data) {


    p1 <- pnorm(nij)

    p0 <- matrix(c, I, J, byrow = TRUE) * (1 - p1)

    p1 <- p1/(p1 + p0)

    rij <- matrix(runif(I * J), nrow = I, ncol = J)

    w <- 1 * (p1 > rij) * data

    return(w)

}

###############################################################################
###############################################################################
.draw.z.3ponb <- function(I, J, w, nij, dat.resp) {


    uij <- matrix(runif(I * J), nrow = I, ncol = J)

    ZZ <- 1000

    tw <- -ZZ + ZZ * w

    tw <- tw - (1 - dat.resp) * ZZ

    tp <- ZZ * w

    tp <- tp + (1 - dat.resp) * ZZ

    pl <- pnorm(tw, mean = nij)

    pu <- pnorm(tp, mean = nij)

    pij <- pl + (pu - pl) * uij

    z <- qnorm(pij, mean = nij)

    return(z)

}

###############################################################################
###############################################################################
.draw.theta.3ponb <- function(am, z, bm, mu, sigma, I, J) {


    v <- 1/rowSums(am^2)

    thetat <- rowSums(am * (z + bm)) * v

    vtheta <- 1/((1/v) + (1/sigma^2))

    mtheta <- ((thetat/v) + (mu/sigma^2)) * vtheta

    theta <- rnorm(I, mean = mtheta, sd = sqrt(vtheta))

    theta <- matrix(theta, nrow = I, ncol = 1, byrow = FALSE)

    return(theta)

}

###############################################################################
###############################################################################
.draw.ab.3ponb <- function(theta, z, J) {


    x <- cbind(theta, -1)

    vab <- solve(t(x) %*% x)

    mab <- vab %*% t(x) %*% z

    ab <- mvtnorm::rmvnorm(J, sigma = vab) + as.matrix(t(mab))

    colnames(ab) <- c("a", "b")

    return(ab)

}

###############################################################################
###############################################################################
.draw.c.3ponb <- function(w, data, dat.resp, J, guess.priori) {


    ti <- colSums((w == 0))

    si <- colSums((w == 0) * (data == 1) * dat.resp)

    c <- rbeta(J, si + guess.priori[, 1], ti - si + guess.priori[, 2])

    return(c)

}

###############################################################################
###############################################################################
.deviance.3pnob <- function(am, bm, theta, c, data, dat.resp, nij, eps) {


    cm <- matrix(c, nrow = nrow(am), ncol = length(c))

    pij <- cm + (1 - cm) * pnorm(nij)

    llij <- log(dat.resp * (data * pij + (1 - data) * (1 - pij)) + eps)

    deviance <- -2 * sum(llij)

    return(deviance)

}


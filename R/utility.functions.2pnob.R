#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#utility functions for mcmc.2pnob
#
###############################################################################
###############################################################################
.iniv.2ponb <- function(J, data0) {

  a <- rep(1, J)

  b <- qnorm((colMeans(data0, na.rm = TRUE) + 0.01)/1.02)

  theta <- qnorm((rowMeans(data0, na.rm = TRUE) + 0.01)/1.02)

  # Initial values-----------------------------------------

  v.inicia <- list("a" = a, "b" = b, "theta" = theta)

  return(v.inicia)

}

###############################################################################
###############################################################################

.interval <- function(iter, burning, thin) {

  int <- seq(thin, iter, by = thin)

  # Interval for Save Values ------------------------------

  int <- int[int > burning]

  return(int)

}

###############################################################################

.draw.z.2ponb <- function(I, J, nij, tw, tp) {

  uij <- matrix(runif(I * J), nrow = I, ncol = J)

  pl <- pnorm(tw, mean = nij)

  pu <- pnorm(tp, mean = nij)

  pij <- pl + (pu - pl) * uij

  z <- qnorm(pij, mean = nij)

  return(z)

}

###############################################################################
###############################################################################

.draw.theta.2ponb <- function(am, z, bm, I, J) {

  v <- 1/(rowSums(am^2) + 1)

  m <- rowSums(am * (z + bm)) * v

  theta <- rnorm(I, mean = m, sd = sqrt(v))

  theta <- matrix(theta, nrow = I, ncol = 1, byrow = FALSE)

  return(theta)

}

###############################################################################
###############################################################################

.draw.ab.2ponb <- function(theta, z, J) {

  x <- as.matrix(cbind(theta, -1))

  vab <- solve(t(x) %*% x)

  mab <- vab %*% t(x) %*% z

  ab <- mvtnorm::rmvnorm(J, sigma = vab) + as.matrix(t(mab))

  colnames(ab) <- c("a", "b")

  return(ab)

}

###############################################################################
###############################################################################

.deviance.2pnob <- function(data, dat.resp, nij, eps) {

  pij <- pnorm(nij)

  llij <- log(dat.resp * (data * pij + (1 - data) * (1 - pij)) + eps)

  deviance <- -2 * sum(llij)

  return(deviance)

}


.mcmc.2pnob.shiny <- function(data, initial.value = NULL, iter = 1000,
                              burning = 500, thin = 1, studyTheta = TRUE,
                              parts = 3, ...){


  .test (iter, burning, thin, parts)

  .initial.value.test (data, initial.value)

  pro.iteration <- round(iter - burning) / 10

  pro <- seq(1, iter, pro.iteration)

  data0 <- data

  data <- as.matrix(data)

  data[ is.na(data0) ] <- 0

  dat.resp <- 1-is.na( data0 )

  I <- nrow( data )

  J <- ncol( data )

  eps <- 10 ^ ( -10 )

  # Initial Values ============================================================

  if (is.null(initial.value) == TRUE ){

    initial.value <- .iniv.2ponb (J, data0)

  }

  a <- matrix(initial.value$a, nrow = 1, byrow = TRUE)

  b <- as.vector(initial.value$b)

  theta <- matrix(initial.value$theta, nrow = I, ncol = 1, byrow = FALSE)

  am <- matrix(as.vector(initial.value$a), nrow = I, ncol = J, byrow = TRUE)

  bm <- matrix(b, nrow = I, ncol = J, byrow = TRUE)

  ZZ <- 1000

  tw <- -ZZ + ZZ* data

  tw[ is.na( data0 ) ] <- -ZZ

  tp <- ZZ* data

  tp[ is.na( data0 ) ] <- ZZ


  # Save Values ===============================================================

  ii <- 0

  svindex <- .interval (iter, burning, thin)

  a.chain <- b.chain <- matrix(NA, length(svindex), J)

  d.chain<- rep(NA, length(svindex))

  theta.chain <- matrix(NA , length(svindex), I)

  zz <- 0

  # Iteration =================================================================

  ###
  withProgress(message = "Start",value = 0, {

    for (i in 1:iter) {

      nij <- theta %*% a - bm


      # Draw z ----------------------------------------------

      z <- .draw.z.2ponb(I, J, nij, tw, tp)

      # Draw theta ------------------------------------------

      theta <- .draw.theta.2ponb(am, z, bm, I, J)

      # Draw a y b ------------------------------------------

      ab <- .draw.ab.2ponb(theta, z, J)

      a <- ab[, 1]

      b <- ab[, 2]


      # Defining values -------------------------------------

      am <- matrix(a, nrow = I, ncol = J, byrow = TRUE)

      bm <- matrix(b, nrow = I, ncol = J, byrow = TRUE)

      # Save Values -----------------------------------------

      if (i %in% svindex) {

        ii <- ii + 1

        a.chain[ii, ] <- a

        b.chain[ii, ] <- b

        theta.chain[ii, ] <- theta

        d.chain[ii] <- .deviance.2pnob(data, dat.resp, nij, eps)

      }


      # Progress --------------------------------------------

      if ( ( i %%  pro.iteration ) == 0 ){

        incProgress( 1 / length( pro ),
                     message = "MCMC", detail = paste( "Iteration", i ) )

      }

      # End iteration =========================================================

    }

  })
  ###

  # Final values ------------------------------------------

  final.values <- list("a" = a.chain[nrow(a.chain), ],
                       "b" = b.chain[nrow(b.chain), ],
                       "theta" = theta.chain[nrow(theta.chain),])

  # MCMC Objet --------------------------------------------

  mcmcobj <- list("a" = a.chain, "b" = b.chain, "theta" = theta.chain)

  # Diagnostic --------------------------------------------

  diag <- .diag.mcmcobj(mcmcobj, parts, ...)

  # MCMC list ---------------------------------------------

  mcmclist <- list("mcmcobj" = mcmcobj,
                   "diagnostic" = list("diag" = diag,
                                       "deviance" = mean(d.chain)),
                   "information" = list("final.values" = final.values,
                                        "iter" = iter,
                                        "burning" = burning,
                                        "data" = data,
                                        "thin" = thin,
                                        "parts" = parts,
                                        "model" = "2pnob"))


  # mcmcBairt class ---------------------------------------

  class(mcmclist) <- append("mcmc.2pnob", "bairt")

  return(mcmclist)

}

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


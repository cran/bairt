.mcmc.3pnob.shiny <- function(data, initial.value = NULL, c.prior = c(1,1),
                              iter = 1000, burning = 10, thin = 1,
                              studyTheta = TRUE, parts = 3, ...){


  .test (iter, burning, thin, parts)

  .initial.value.test (data, initial.value)

  pro.iteration <- round(iter - burning)/ 10

  pro <- seq(1, iter, pro.iteration)

  data0 <- data

  data <- as.matrix(data)

  data[ is.na(data0) ] <- 0

  dat.resp <- 1 - is.na(data0)

  I<- nrow(data)

  J<- ncol(data)

  mu <- 0

  sigma <- 1

  eps <- 10^(-10)

  # Initial Values ============================================================

  if (is.null(initial.value) == TRUE){

    initial.value <- .iniv.3ponb(c.prior, J, data0)

  }

  a <- matrix(initial.value$a, nrow = 1, byrow = TRUE )

  b <- as.vector(initial.value$b)

  c <- as.vector(initial.value$c)

  theta <- matrix(initial.value$theta, nrow = I, ncol=1, byrow = FALSE )

  am <- matrix(as.vector(initial.value$a), nrow = I, ncol = J , byrow = TRUE)

  bm <- matrix(b, nrow = I, ncol = J, byrow = TRUE )

  guess.priori <- matrix(c.prior, nrow = J, ncol = 2, byrow = TRUE )

  # Save Values ===============================================================

  ii <- 0

  svindex <- .interval(iter, burning, thin)

  a.chain <- matrix(NA , length( svindex ), J)

  c.chain <- b.chain <- a.chain

  d.chain <- rep(NA, length(svindex))

  theta.chain <- matrix(NA , length(svindex) , I)

  # Iteration =================================================================

  ####
  withProgress(message = "Start", value = 0, {

    for (i in 1:iter) {

      nij <- theta %*% a - bm

      # Draw w ----------------------------------------------

      w <- .draw.w.3ponb(nij, c, I, J, data)

      # Draw z ----------------------------------------------

      z <- .draw.z.3ponb(I, J, w, nij, dat.resp)

      # Draw theta ------------------------------------------

      theta <- .draw.theta.3ponb(am, z, bm, mu, sigma, I, J)

      # Draw a y b ------------------------------------------

      ab <- .draw.ab.3ponb(theta, z, J)
      a <- ab[, 1]
      b <- ab[, 2]

      # Draw c ----------------------------------------------

      c <- .draw.c.3ponb(w, data, dat.resp, J, guess.priori)

      # Defining values -------------------------------------

      am <- matrix(a, nrow = I, ncol = J, byrow = TRUE)

      bm <- matrix(b, nrow = I, ncol = J, byrow = TRUE)

      # Save Values -----------------------------------------

      if (i %in% svindex) {

        ii <- ii + 1

        a.chain[ii, ] <- a

        b.chain[ii, ] <- b

        c.chain[ii, ] <- c

        theta.chain[ii, ] <- theta

        d.chain[ii] <- .deviance.3pnob(am, bm, theta, c, data, dat.resp,
                                       nij, eps)

      }

      # Progress ---------------------------------------------

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
                       "c" = c.chain[nrow(c.chain), ],
                       "theta" = theta.chain[nrow(theta.chain), ])

  # MCMC Objet --------------------------------------------

  mcmcobj <- list("a" = a.chain, "b" = b.chain, "c" = c.chain,
                  "theta" = theta.chain)

  # Diagnostic --------------------------------------------

  diag <- .diag.mcmcobj(mcmcobj, parts, ...)


  mcmclist <- list("mcmcobj" = mcmcobj,
                   "diagnostic" = list("diag" = diag,
                                       "deviance" = mean(d.chain)),
                   "information" = list("final.values" = final.values,
                                        "c.prior" = c.prior,
                                        "iter" = iter,
                                        "burning" = burning,
                                        "data" = data,
                                        "thin" = thin,
                                        "parts" = parts,
                                        "model" = "3pnob"))

  # mcmcBairt class ---------------------------------------

  class(mcmclist) <- append("mcmc.3pnob", "bairt")

  return(mcmclist)

}

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


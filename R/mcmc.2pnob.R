#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#mcmc.2pnob
#
###############################################################################
###############################################################################
#' @title MCMC Estimation of the Two-Parameter Normal Ogive Model
#'
#' @description
#' This function estimates the Two-Parameter normal ogive item response model
#' by MCMC sampling (Johnson & Albert, 1999, p. 195). It is a modification of
#' the function \emph{mcmc.2pno} of \emph{sirt} package.
#'
#' @usage
#' mcmc.2pnob(data, initial.value = NULL, iter = 1000,
#'            burning = 500, thin = 1, parts = 3, ...)
#'
#' @param data Data frame with dichotomous item responses.
#' @param initial.value List with initial values
#' @param iter Total number of iterations.
#' @param burning Number of burnin iterations.
#' @param thin The thinning interval between consecutive observations.
#' @param parts Number of splits for MCMC chain.
#' @param ... Further arguments.
#'
#' @return
#' An object of class \emph{mcmc.2pnob}. This is a list with the following
#' elements:
#'   \item{mcmcobj}{A list with the \emph{a}, \emph{b}, y \emph{theta} chains.}
#'   \item{diagnostic}{A list with the \emph{diag} matrix (it is a summary whit
#'   Rhat included) and the residual \emph{deviance}.}
#'   \item{information}{A list with the \emph{final.values} (values of the last
#'   iteration for each chain), and the arguments \emph{iter}, \emph{burning},
#'   \emph{data}, \emph{thin}, \emph{parts} and \emph{model}, respectively.}
#'
#' @details
#' For the two-parameter normal ogive item response model, we assume that the
#' performance of the \emph{i-th} examine depends on an unknown latent
#' variable \eqn{\theta_i}, and we let \eqn{\theta_1, ...,\theta_n}
#' respectively denotes the latent traits for all the \emph{n} individuals
#' taking the test.
#'
#' We also assume that the probability of right answer depends only on the
#' latent trait value and on the characteristics of the item. Specifically,
#' for the \emph{i-th} individual and \emph{j-th} item, we model this
#' probability as:
#'
#' \deqn{Pr( Y_{ ij } = 1 | \theta_i, a_ j, b_ j ) =
#' \Phi( a_ j\theta_i - b_ j )}
#'
#' where \eqn{\Phi} is the standard normal cdf, and \eqn{a_ j} and \eqn{b_ j}
#' are the item discrimination and item difficulty parameters associated with
#' the \emph{j-th} item (Johnson & Albert, 1999, p. 188).
#'
#' @author
#' Javier Martínez
#'
#' The code is adapted from an R script of Alexander Robitzsch.
#' (\url{https://github.com/alexanderrobitzsch/sirt/blob/master/R/mcmc.2pno.R})
#'
#' @references
#' Johnson, V. E. & Albert, J. H. (1999). Ordinal Data Modeling.
#' New York: Springer.
#'
#' @seealso
#' \code{\link{mcmc.3pnob}}, \code{\link{continue.mcmc.bairt}},
#' \code{\link[coda]{gelman.diag}} and \code{\link[coda]{as.mcmc}}.
#'
#' @examples
#' # data for model
#' data("MathTest")
#'
#' # estimate model only for the first 500 examinees of the data MathTest
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 400, burning = 100)
#' # study of chains convergence
#' check.plot(model2)
#' diagnostic.mcmc(model2)
#' parameter.plot(model2)
#' chain.study(model2, parameter = "b", chain = 14)
#' irc(model2, item = 3)
#'
#' \donttest{
#' # continue the  MCMC
#' # form 1
#' initialValues <- final.values.mcmc(model2)
#' model21 <- mcmc.2pnob(MathTest[1:500,], initial.value = initialValues,
#' iter = 3000, burning = 0)
#'
#' # form 2
#' model22 <- continue.mcmc(model2, iter = 3000, burning = 0)
#' }
#'
#' ## End(Not run)
#'
#' @export mcmc.2pnob
#' @exportClass mcmc.2pnob
#' @importFrom coda gelman.diag as.mcmc mcmc.list
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats acf pnorm qnorm quantile rbeta rnorm runif spline


mcmc.2pnob <- function(data, initial.value = NULL, iter = 1000, burning = 500,
                       thin = 1, parts = 3, ...) {

  .test(iter, burning, thin, parts)

  .initial.value.test(data, initial.value)

  pro.iteration <- round(iter - burning)/10

  data0 <- data

  data <- as.matrix(data)

  data[is.na(data0)] <- 0

  dat.resp <- 1 - is.na(data0)

  I <- nrow(data)

  J <- ncol(data)

  eps <- 10^(-10)

  # Initial Values ============================================================

  if (is.null(initial.value)) {

    initial.value <- .iniv.2ponb(J, data0)

  }

  a <- matrix(initial.value$a, nrow = 1, byrow = TRUE)

  b <- as.vector(initial.value$b)

  theta <- matrix(initial.value$theta, nrow = I, ncol = 1, byrow = FALSE)

  am <- matrix(as.vector(initial.value$a), nrow = I, ncol = J, byrow = TRUE)

  bm <- matrix(b, nrow = I, ncol = J, byrow = TRUE)

  ZZ <- 1000

  tw <- -ZZ + ZZ * data

  tw[is.na(data0)] <- -ZZ

  tp <- ZZ * data

  tp[is.na(data0)] <- ZZ

  # Save Values ===============================================================

  ii <- 0

  svindex <- .interval(iter, burning, thin)

  a.chain <- b.chain <- matrix(NA, length(svindex), J)

  d.chain <- rep(NA, length(svindex))

  theta.chain <- matrix(NA, length(svindex), I)

  zz <- 0

  # Iteration =================================================================

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

    if ((i%%pro.iteration) == 0) {

      cat("Iteration", i, " | ", paste(Sys.time()), "\n")

    }

    # End iteration ===========================================================

  }

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


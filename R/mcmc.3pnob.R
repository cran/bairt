#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#mcmc.3pnob
#
###############################################################################
###############################################################################
#' @title MCMC Estimation of the Three-Parameter Normal Ogive Model
#'
#' @description
#' This function estimates the Three-Parameter normal ogive item response model
#' by MCMC sampling (Beguin & Glas, 2001, p. 542). It is a modification of the
#' function \emph{mcmc.3pno.testlet} of the \emph{sirt} package.
#'
#' @usage
#' mcmc.3pnob(data, initial.value = NULL, c.prior = c(1, 1), iter = 1000,
#'            burning = 500, thin = 1, parts = 3, ...)
#'
#' @param data Data frame with dichotomous item responses.
#' @param initial.value List with initial values
#' @param c.prior A vector of length two which defines the beta prior
#' distribution of guessing parameters. The default is a non-informative prior,
#' \emph{Beta(1,1)}.
#' @param iter Total number of iterations.
#' @param burning Number of burnin iterations.
#' @param thin The thinning interval between consecutive observations.
#' @param parts Number of splits for MCMC chain.
#' @param ... Further arguments.
#'
#' @return
#' An object of class \emph{mcmc.3pnob.} This is a list with the following
#' elements:
#'   \item{mcmcobj}{A list with the \emph{a}, \emph{b}, y \emph{theta} chains.}
#'   \item{diagnostic}{A list with the \emph{diag} matrix (it is a summary whit
#'   Rhat included) and the residual \emph{deviance}.}
#'   \item{information}{A list with the \emph{final.values} (values of the last
#'   iteration for each chain), and the arguments \emph{c.prior}, \emph{iter},
#'   \emph{burning}, \emph{data}, \emph{thin}, \emph{parts} and \emph{model},
#'   respectively.}
#'
#' @details
#' For the Three-parameter normal ogive item response model, we assume that the
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
#'  \deqn{Pr( Y_{ ij } = 1 | \theta_i, a_ j, b_ j, c_ j ) =
#'  c_ j + (1 - c_ j )\Phi( a_ j\theta_i - b_ j )}
#'
#' where \eqn{\Phi} is the standard normal cdf, and \eqn{a_ j}, \eqn{b_ j} and
#' \eqn{c_ j} are the item discrimination, item difficulty and  item guessing
#' parameters associated with the \emph{j-th} item
#' (Beguin & Glas, 2001, p. 542).
#'
#' @author
#' Javier Martínez
#'
#' The code is adapted from an R script of Alexander Robitzsch.
#' (\url{https://github.com/alexanderrobitzsch/sirt/blob/master/R/mcmc.3pno.testlet.R})
#'
#' @references
#' Beguin, A, A. & Glas, C.A.W. (2001). MCMC Estimation and Some
#' Model-Fit Analysis of Multidimensional IRT Models. Psychometrika,
#' 66, 541-562.
#'
#' @seealso
#' \code{\link{mcmc.2pnob}}, \code{\link{continue.mcmc.bairt}},
#' \code{\link[coda]{gelman.diag}} and \code{\link[coda]{as.mcmc}}.
#'
#' @examples
#' # data for model
#' data("MathTest")
#'
#' # estimate model only for the first 500 examinees of the data MathTest
#' # selection of the prior for 5 response options
#' cprior <- select.c.prior(5)
#' # estimate model only for the first 500 examinees of the data MathTest
#' model3 <- mcmc.3pnob(MathTest[1:500,], iter = 300, burning = 0,
#'                     c.prior = cprior)
#'
#' # study of chains convergence model3
#' check.plot(model3)
#' diagnostic.mcmc(model3)
#' parameter.plot(model3)
#' chain.study(model3, parameter = "a", chain = 15)
#' irc(model3, item = 1)
#'
#' \donttest{
#' # continue the  MCMC
#' # form 1
#' initialValues2 <- final.values.mcmc(model3)
#' model31 <- mcmc.3pnob(MathTest[1:500,], initial.value = initialValues2,
#'                      iter = 3000, burning = 0, c.prior = cprior)
#' # form 2
#' model32 <- continue.mcmc(model3, iter = 3000, burning = 0)
#' }
#'
#' ## End(Not run)
#'
#' @export mcmc.3pnob
#' @exportClass mcmc.3pnob
#' @importFrom coda gelman.diag as.mcmc mcmc.list
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats acf pnorm qnorm quantile rbeta rnorm runif spline


mcmc.3pnob <- function(data, initial.value = NULL, c.prior = c(1, 1),
                       iter = 1000, burning = 500, thin = 1, parts = 3, ...) {


    .test(iter, burning, thin, parts)

    .initial.value.test(data, initial.value)

    pro.iteracion <- round(iter - burning) / 10

    data0 <- data

    data <- as.matrix(data)

    data[is.na(data0)] <- 0

    dat.resp <- 1 - is.na(data0)

    I <- nrow(data)

    J <- ncol(data)

    mu <- 0

    sigma <- 1

    eps <- 10^(-10)

    # Initial Values ==========================================================

    if (is.null(initial.value)) {

        initial.value <- .iniv.3ponb(c.prior, J, data0)

    }

    a <- matrix(initial.value$a, nrow = 1, byrow = TRUE)

    b <- as.vector(initial.value$b)

    c <- as.vector(initial.value$c)

    theta <- matrix(initial.value$theta, nrow = I, ncol = 1, byrow = FALSE)

    am <- matrix(as.vector(initial.value$a), nrow = I, ncol = J, byrow = TRUE)

    bm <- matrix(b, nrow = I, ncol = J, byrow = TRUE)

    guess.priori <- matrix(c.prior, nrow = J, ncol = 2, byrow = TRUE)

    # Save Values =============================================================

    ii <- 0

    svindex <- .interval(iter, burning, thin)

    a.chain <- matrix(NA, length(svindex), J)

    c.chain <- b.chain <- a.chain

    d.chain <- rep(NA, length(svindex))

    theta.chain <- matrix(NA, length(svindex), I)

    # Iteration ===============================================================

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

        if ((i%%pro.iteracion) == 0) {

            cat("Iteration", i, " | ", paste(Sys.time()), "\n")

        }

        # End iteration =======================================================

    }

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



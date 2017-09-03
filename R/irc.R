#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#irc
#
###############################################################################
###############################################################################
#' @title Plot of posterior density of the item response curve
#'
#' @description
#' Plot of posterior density of the item response curve for the \emph{j-th}
#' \emph{item}.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' Plot of posterior density of the item response curve for de \emph{j-th}
#' \code{item}.
#'
#' @details
#' The solid line corresponds to the location of the posterior mean and the
#' points correspond to the percentiles determined for \emph{prob}.
#' If \emph{prob = c(0.05, 0.95)} is equivalent to the 5th and 95th percentils
#' of the posterior density.
#'
#' @author
#' Javier Martínez
#'
#' @references
#' Johnson, V. E. & Albert, J. H. (1999). Ordinal Data Modeling.
#' New York: Springer.
#'
#' @seealso
#' \code{\link{mcmc.2pnob}}, \code{\link{mcmc.3pnob}} and
#' \code{\link{continue.mcmc.bairt}}.
#'
#' @export irc
#' @exportMethod  irc

irc <- function(mcmclist, ...) UseMethod("irc", mcmclist)

###############################################################################
###############################################################################
#' @title Plot of posterior density of the item response curve
#'
#' @description
#' Plot of posterior density of the item response curve for the \emph{j-th}
#' \emph{item}.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' Plot of posterior density of the item response curve for de \emph{j-th}
#' \code{item}.
#'
#' @details
#' The solid line corresponds to the location of the posterior mean and the
#' points correspond to the percentiles determined for \emph{prob}.
#' If \emph{prob = c(0.05, 0.95)} is equivalent to the 5th and 95th percentils
#' of the posterior density.
#'
#' @author
#' Javier Martínez
#'
#' @references
#' Johnson, V. E. & Albert, J. H. (1999). Ordinal Data Modeling.
#' New York: Springer.
#'
#' @seealso
#' \code{\link{mcmc.2pnob}}, \code{\link{mcmc.3pnob}} and
#' \code{\link{continue.mcmc.bairt}}.
#'
#' @export

irc.default <- function(mcmclist, ...) NULL

###############################################################################
###############################################################################
#' @title Plot of posterior density of the item response curve
#'
#' @description
#' Plot of posterior density of the item response curve for the \emph{j-th}
#' \emph{item}.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param item The number of \emph{j-th} item.
#' @param color Item response curve color.
#' @param prob A vector of length two for defined the percentiles of the
#' posterior density.
#' @param ... Further arguments.
#'
#' @return
#' Plot of posterior density of the item response curve for de \emph{j-th}
#' \code{item}.
#'
#' @details
#' The solid line corresponds to the location of the posterior mean and the
#' points correspond to the percentiles determined for \emph{prob}.
#' If \emph{prob = c(0.05, 0.95)} is equivalent to the 5th and 95th percentils
#' of the posterior density.
#'
#' @author
#' Javier Martínez
#'
#' @references
#' Johnson, V. E. & Albert, J. H. (1999). Ordinal Data Modeling.
#' New York: Springer.
#'
#' @seealso
#' \code{\link{mcmc.2pnob}}, \code{\link{mcmc.3pnob}} and
#' \code{\link{continue.mcmc.bairt}}.
#'
#' @examples
#' # data for model
#' data("MathTest")
#'
#' # Only for the first 500 examinees of the data MathTest
#' # Two-Parameter Normal Ogive Model
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 400, burning = 100)
#' check.plot(model2)
#' irc(model2, item = 3)
#'
#' \donttest{
#' # For all examinees of the data
#' # Three-Parameter Normal Ogive Model
#' # selection of the prior for 5 response options
#' cprior <- select.c.prior(5)
#' modelAll3 <- mcmc.3pnob(MathTest, iter = 1000, burning = 0,
#'                     c.prior = cprior)
#' irc(modelAll3 , item = 1, color = "blue")
#' irc(modelAll3 , item = 1, color = "blue", prob = c(0.1, 0.9))
#' }
#'
#' ## End(Not run)
#'
#'
#' @export
#' @importFrom graphics abline boxplot hist legend lines par plot points
#' segments text

irc.bairt <- function(mcmclist, item = 1, color = "red",
                      prob = c(0.05, 0.95), ...) {

    # Check in ================================================================

    if (length(prob) != 2){

    stop(paste0("prob is not length 2"), call. = FALSE)

    }

    for (i in 1:2) {

        if (prob[i] < 0 || prob[i] > 1) {

            stop(paste0("prob has a value < 0 or > 1"), call. = FALSE)

        }

    }

    .item.test(mcmclist, item)

    diag <- mcmclist$diagnostic$diag

    diagMean <- diag[, c("Parameter", "Chain", "Mean")]

    diaglist <- split(diagMean, diag$Parameter)

    theta <- diaglist$theta$Mean

    pij <- .prob.pno(mcmclist, prob)

    # Plot ====================================================================

    plot(theta, pij$Mean[, item], ylab = "Probability of Correct Response",
         xlab = "Theta", type = "n", cex = 2, ...)

    # Poins ----------------------------------------------------

    points(x = theta, y = pij[[1]][, item], cex = 0.1, pch = ".")

    points(x = theta, y = pij[[2]][, item], cex = 0.1, pch = ".")

    # Line -----------------------------------------------------

    lines(spline(theta, pij$Mean[, item], n = 201), lty = 1, col = color, lwd = 2)

}


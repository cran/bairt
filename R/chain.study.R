#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#chain.study
#
###############################################################################
###############################################################################
#' @title Convergence graphs for the simulated values
#'
#' @description
#' Convergence graphs for the study of the simulated values for an MCMC
#' marginal chain.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' Convergence graphs for the study of the simulated values for an MCMC
#' marginal chain.
#'
#' @details
#' The top left graph displays the sequence of simulated values and the top
#' right graph displays the lagged correlations of the sequence as a function of
#' the lag value. The bottom left graph is an histogram of the simulated values
#' and the bottom right graph is the box plot of the simulated values.
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
#' \code{\link{continue.mcmc}}.
#'
#' @export chain.study
#' @exportMethod chain.study

chain.study <- function(mcmclist, ...) UseMethod("chain.study", mcmclist)

###############################################################################
###############################################################################
#' @title Convergence graphs for the simulated values
#'
#' @description
#' Convergence graphs for the study of the simulated values for an MCMC
#' marginal chain.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' Convergence graphs for the study of the simulated values for an MCMC
#' marginal chain.
#'
#' @details
#' The top left graph displays the sequence of simulated values and the top
#' right graph displays the lagged correlations of the sequence as a function of
#' the lag value. The bottom left graph is an histogram of the simulated values
#' and the bottom right graph is the box plot of the simulated values.
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
#' \code{\link{continue.mcmc}}.
#'
#' @export


chain.study.default <- function(mcmclist, ...) NULL

###############################################################################
###############################################################################
#' @title Convergence graphs for the simulated values
#'
#' @description
#' Convergence graphs for the study of the simulated values for an MCMC
#' marginal chain.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param parameter The parameter (\emph{a}, \emph{b}, \emph{c} or
#' \emph{theta}) for graphing.
#' @param chain The number of the chain that will be graphed.
#' @param line A red line that represent the posterior mean of the
#' simulated values.
#' @param ... Further arguments.
#'
#' @return
#' Convergence graphs for the study of the simulated values for an MCMC
#' marginal chain.
#'
#' @details
#' The top left graph displays the sequence of simulated values and the top
#' right graph displays the lagged correlations of the sequence as a function of
#' the lag value. The bottom left graph is an histogram of the simulated values
#' and the bottom right graph is the box plot of the simulated values.
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
#' \code{\link{continue.mcmc}}.
#'
#' @examples
#' # data for model
#' data("MathTest")
#'
#' # Only for the first 500 examinees of the data MathTest
#' # Two-Parameter Normal Ogive Model
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 400, burning = 100)
#' check.plot(model2)
#' chain.study(model2, parameter = "b", chain = 12)
#' chain.study(model2, parameter = "theta", chain = 10)
#'
#' \donttest{
#' # For all examinees of the data MathTest
#' # Two-Parameter Normal Ogive Model
#' modelAll2 <- mcmc.2pnob(MathTest, iter = 3500, burning = 500, thin = 10)
#' check.plot(modelAll2)
#' chain.study(modelAll2, parameter = "b", chain = 14)
#' chain.study(modelAll2, parameter = "theta", chain = 10)
#'
#' # Three-Parameter Normal Ogive Model
#' modelAll3 <- mcmc.3pnob(MathTest, iter = 3500, burning = 500, thin = 10)
#' check.plot(modelAll3)
#' chain.study(modelAll3, parameter = "b", chain = 12)
#' chain.study(modelAll3, parameter = "c", chain = 10)
#' }
#'
#' ## End(Not run)
#'
#' @importFrom graphics abline boxplot hist legend lines par plot points
#' segments text
#'
#' @export

chain.study.bairt <- function(mcmclist, parameter = "a", chain = 1,
                              line = TRUE, ...) {


    # Check in ================================================================

    .parameter.test(mcmclist, parameter)

    .chain.test(mcmclist, parameter, chain)


    # save old par() ==========================================================

    old <- par(no.readonly = TRUE)


    chainm <- mcmclist$mcmcobj[[parameter]][, chain]


    par(mfrow = c(2, 2))

    # plot ====================================================================

    plot(chainm, type = "l", xlab = "Iteration", ylab = "Value",
      main = "SIMULATED VALUES", ...)

    if (line) {

        me <- mean(chainm)

        abline(col = 2, h = me)

    }

    # ACF =====================================================================

    acf(chainm, main = "LAG CORRELATION")

    # Hist ====================================================================

    hist(chainm, main = "DENSITY", xlab = paste("Chain", parameter, chain))

    # Box plot ================================================================

    boxplot(chainm, main = "BOX PLOT", xlab = paste("Chain", parameter, chain))

    #Return old par() =========================================================

    on.exit(par(old))

    invisible()

}


#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#object.coda
#
###############################################################################
###############################################################################
#' @title Creating an mcmc.list for coda package
#'
#' @description
#' The function \emph{object.coda} create a \emph{mcmc.list} object.
#' With this is possible to study the chain using the coda packet.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' A \emph{mcmc.list} coda packet object.
#'
#' @details
#' The function \emph{object.coda} create a \emph{mcmc.list} object of the
#' marginal chain selectionated. The marginal chain is splits in parts
#' determined for \emph{parts}. The objetive is  represent parallel chains
#' with different starting values (Beguin & Glas, 2001, p. 547).
#'
#' @author
#' Javier Martínez
#'
#' @references
#' A.A.  Beguin, A, A. & Glas, C.A.W. (2001). MCMC Estimation and Some
#' Model-Fit Analysis of Multidimensional IRT Models. Psychometrika,
#' 66, 541-562.
#'
#' @seealso
#' \code{\link[coda]{as.mcmc.list}} and \code{\link[coda]{as.mcmc}}.
#'
#' @export object.coda
#' @exportMethod object.coda

object.coda <- function(mcmclist, ...) UseMethod("object.coda", mcmclist)

###############################################################################
###############################################################################
#' @title Creating an mcmc.list for coda package
#'
#' @description
#' The function \emph{object.coda} create a \emph{mcmc.list} object.
#' With this is possible to study the chain using the coda packet.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
##' @return
#' A \emph{mcmc.list} coda packet object.
#'
#' @details
#' The function \emph{object.coda} create a \emph{mcmc.list} object of the
#' marginal chain selectionated. The marginal chain is splits in parts
#' determined for \emph{parts}. The objetive is  represent parallel chains
#' with different starting values (Beguin & Glas, 2001, p. 547).
#'
#' @author
#' Javier Martínez
#'
#' @references
#' A.A.  Beguin, A, A. & Glas, C.A.W. (2001). MCMC Estimation and Some
#' Model-Fit Analysis of Multidimensional IRT Models. Psychometrika,
#' 66, 541-562.
#'
#' @seealso
#' \code{\link[coda]{as.mcmc.list}} and \code{\link[coda]{as.mcmc}}.
#'
#' @export

object.coda.default <- function(mcmclist, ...) NULL

###############################################################################
###############################################################################
#' @title Creating an mcmc.list for coda package
#'
#' @description
#' The function \emph{object.coda} create a \emph{mcmc.list} object.
#' With this is possible to study the chain using the coda packet.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param parameter The parameter (a, b, c or theta) for graphing.
#' @param chain The parameter's chain that will be graphed.
#' @param parts Number of splits for MCMC chain.
#' @param ... Further arguments.
#'
#' @return
#' A \emph{mcmc.list} coda packet object.
#'
#' @details
#' The function \emph{object.coda} create a \emph{mcmc.list} object of the
#' marginal chain selectionated. The marginal chain is splits in parts
#' determined for \emph{parts}. The objetive is  represent parallel chains
#' with different starting values (Beguin & Glas, 2001, p. 547).
#'
#' @author
#' Javier Martínez
#'
#' @references
#' A.A.  Beguin, A, A. & Glas, C.A.W. (2001). MCMC Estimation and Some
#' Model-Fit Analysis of Multidimensional IRT Models. Psychometrika,
#' 66, 541-562.
#'
#' @seealso
#' \code{\link[coda]{as.mcmc.list}} and \code{\link[coda]{as.mcmc}}.
#'
#' @examples
#' # data for model
#' data("MathTest")
#'
#' # Only for the first 500 examinees of the data MathTest
#' # Two-Parameter Normal Ogive Model
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 400, burning = 100)
#'
#' chain_a1 <- object.coda(model2, parameter = "a", chain = 1)
#' coda::gelman.plot(chain_a1)
#' coda::gelman.diag(chain_a1)
#' plot(chain_a1)
#'
#' \donttest{
#' # For all examinees of the data
#' # Three-Parameter Normal Ogive Model
#' # selection of the prior for 5 response options
#' cprior <- select.c.prior(5)
#' model3 <- mcmc.3pnob(MathTest, iter = 3500, burning = 500,
#'                     c.prior = cprior, parts = 3)
#'
#' chain_c1 <- object.coda(model3, parameter = "c", chain = 1)
#' coda::gelman.plot(chain_c1)
#' coda::gelman.diag(chain_c1)
#' plot(chain_c1)
#' }
#'
#' ## End(Not run)
#'
#'
#' @importFrom coda as.mcmc
#' @importFrom coda mcmc.list
#'
#' @export

object.coda.bairt <- function(mcmclist, parameter = "a", chain = 1,
                              parts = NULL, ...) {


    # Check in ================================================================

    .parameter.test(mcmclist, parameter)

    .chain.test(mcmclist, parameter, chain)



    mcmc.chain <- mcmclist$mcmcobj[[parameter]][, chain]


    if (is.null(parts)) {

        parts <- mcmclist$information$parts

    }

    mcmc <- .mcmc.divide(mcmc.chain, parts)

    return(mcmc)

}


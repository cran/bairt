#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#continue.mcmc
#
###############################################################################
###############################################################################
#' @title Continue MCMC for the Estimation of the Two-Parameter or
#' Three-Parameter Normal Ogive Model
#'
#' @description
#' This is a function for \emph{bairt} objects. You can use \emph{continue.mcmc}
#' for continue the MCMC the Two-Parameter or Three-Parameter
#' normal ogive item response model.
#'
#' @param mcmclist A \emph{bairt} class object (\emph{mcmc.2pnob} or
#' \emph{mcmc.3pnob}).
#' @param ... Further arguments.
#'
#' @return
#' An \emph{mcmc.2pnob or mcmc.3pnob} object.
#'
#' @details
#' If any argument (\emph{final.values}, \emph{c.prior}, \emph{iter},
#' \emph{burning}, \emph{thin} or \emph{parts}) is NULL, \emph{continue.mcmc}
#' take the value of the \emph{mcmclist}.
#'
#' @author
#' Javier Martínez
#'
#' @references
#' Johnson, V. E., & Albert, J. H. (1999). Ordinal Data Modeling.
#' New York: Springer.
#'
#' A.A.  Beguin, A, A & Glas, C.A.W. (2001). MCMC Estimation and
#' Some Model-Fit Analysis of Multidimensional IRT Models. Psychometrika,
#' 66, 541-562.
#'
#' @seealso
#' \code{\link{mcmc.2pnob}} and \code{\link{mcmc.3pnob}}.
#'
#' @export continue.mcmc
#' @exportMethod  continue.mcmc

continue.mcmc <- function(mcmclist, ...) UseMethod("continue.mcmc", mcmclist)

###############################################################################
###############################################################################
#' @title Continue MCMC for the Estimation of the Two-Parameter or
#' Three-Parameter Normal Ogive Model
#'
#' @description
#' This is a function for \emph{bairt} objects. You can use \emph{continue.mcmc}
#' for continue the MCMC the Two-Parameter or Three-Parameter
#' normal ogive item response model.
#'
#' @param mcmclist A \emph{bairt} class object (\emph{mcmc.2pnob} or
#' \emph{mcmc.3pnob}).
#' @param ... Further arguments.
#'
#' @return
#' An \emph{mcmc.2pnob or mcmc.3pnob} object.
#'
#' @details
#' If any argument (\emph{final.values}, \emph{c.prior}, \emph{iter},
#' \emph{burning}, \emph{thin} or \emph{parts}) is NULL, \emph{continue.mcmc}
#' take the value of the \emph{mcmclist}.
#'
#' @author
#' Javier Martínez
#'
#' @references
#' Johnson, V. E., & Albert, J. H. (1999). Ordinal Data Modeling.
#' New York: Springer.
#'
#' A.A.  Beguin, A, A & Glas, C.A.W. (2001). MCMC Estimation and
#' Some Model-Fit Analysis of Multidimensional IRT Models. Psychometrika,
#' 66, 541-562.
#'
#' @seealso
#' \code{\link{mcmc.2pnob}} and \code{\link{mcmc.3pnob}}.
#'
#' @export

continue.mcmc.default <- function(mcmclist, ...) NULL

###############################################################################
###############################################################################
#' @title Continue MCMC for the Estimation of the Two-Parameter or
#' Three-Parameter Normal Ogive Model
#'
#' @description
#' This is a function for \emph{bairt} objects. You can use \emph{continue.mcmc}
#' for continue the MCMC the Two-Parameter or Three-Parameter
#' normal ogive item response model.
#'
#' @param mcmclist A \emph{bairt} class object (\emph{mcmc.2pnob} or
#' \emph{mcmc.3pnob}).
#' @param initial.value List with initial values.
#' @param c.prior A two dimensional vector which defines the beta prior
#' distribution of guessing parameters. The default is a non-informative prior,
#' \emph{Beta(1,1)}.
#' @param iter Total number of iterations.
#' @param burning Number of burnin iterations.
#' @param thin The thinning interval between consecutive observations.
#' @param parts Number of splits for MCMC chain.
#' @param ... Further arguments.
#'
#' @return
#' An \emph{mcmc.2pnob or mcmc.3pnob} object.
#'
#' @details
#' If any argument (\emph{final.values}, \emph{c.prior}, \emph{iter},
#' \emph{burning}, \emph{thin} or \emph{parts}) is NULL, \emph{continue.mcmc}
#' take the value of the \emph{mcmclist}.
#'
#' @author
#' Javier Martínez
#'
#' @references
#' Johnson, V. E., & Albert, J. H. (1999). Ordinal Data Modeling.
#' New York: Springer.
#'
#' A.A.  Beguin, A, A & Glas, C.A.W. (2001). MCMC Estimation and
#' Some Model-Fit Analysis of Multidimensional IRT Models. Psychometrika,
#' 66, 541-562.
#'
#' @seealso
#' \code{\link{mcmc.2pnob}} and \code{\link{mcmc.3pnob}}.
#'
#' @examples
#' # data for model
#' data("MathTest")
#'
#' # Only for the first 500 examinees of the data MathTest
#' # Two-Parameter Normal Ogive Model
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 100, burning = 0)
#'
#' # continue the  MCMC for the Two-Parameter Normal Ogive Model
#' model21 <- continue.mcmc(model2, iter = 100, burning = 0)
#'
#' \donttest{
#' # For all examinees of the data MathTest
#' # Three-Parameter Normal Ogive Model
#' # selection of the prior for 5 response options
#' cprior <- select.c.prior(5)
#' modelAll3 <- mcmc.3pnob(MathTest, iter = 1000, burning = 0,
#'                     c.prior = cprior)
#'
#' #continue the  MCMC for the Three-Parameter Normal Ogive Model
#' # form 1
#' initialValues2 <- final.values.mcmc(modelAll3)
#' modelAll31 <- mcmc.3pnob(MathTest, initial.value = initialValues2,
#'                      iter = 2000, burning = 0, c.prior = cprior)
#' # form 2
#' modelAll32 <- continue.mcmc(modelAll3, iter = 2000, burning = 0)
#' }
#'
#' ## End(Not run)
#'
#'
#' @importFrom coda gelman.diag
#' @importFrom coda as.mcmc
#' @importFrom coda mcmc.list
#' @importFrom mvtnorm rmvnorm
#' @export

continue.mcmc.bairt <- function(mcmclist, initial.value = NULL, c.prior = NULL,
                                iter = NULL, burning = NULL, thin = NULL,
                                parts = NULL, ...) {

    data <- mcmclist$information$data


    if (is.null(initial.value)) {

        initial.value <- mcmclist$information$final.values

    }


    if (is.null(iter)) {

        iter <- mcmclist$information$iter

    }

    if (is.null(burning)) {

        burning <- mcmclist$information$burning

    }

    if (is.null(thin)) {

        thin <- mcmclist$information$thin

    }

    if (is.null(parts)) {

        parts <- mcmclist$information$parts

    }


    if (is.null(c.prior)) {

        c.prior <- mcmclist$information$c.prior

    }


    if ("mcmc.2pnob" %in% class(mcmclist)) {

        out <- mcmc.2pnob(data, initial.value, iter, burning, thin, parts, ...)

    }

    if ("mcmc.3pnob" %in% class(mcmclist)) {

        out <- mcmc.3pnob(data, initial.value, c.prior, iter, burning,
                         thin, parts, ...)

    }

    return(out)

}




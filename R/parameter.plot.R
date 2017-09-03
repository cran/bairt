#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#parameter.plot
#
###############################################################################
###############################################################################
#' @title Graph of marginal posterior densities
#'
#' @description
#' Graph of marginal posterior densities for the item parameters (\emph{a},
#' \emph{b} or \emph{c}).
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' Graph of marginal posterior densities for the item parameters (\emph{a},
#' \emph{b} or \emph{c}).
#'
#' @details
#' Graph of marginal posterior densities of the item parameter \emph{a, b}
#' for \emph{mcmc.2pnob} object or \emph{a, b, c} for \emph{mcmc.3pnob object}.
#' The center of error bar corresponds to the marginal posterior mean and
#' the extremes correspond to percentiles of the marginal posterior density
#' (These are delimited for \emph{prob}). For example,
#' \emph{prob = c(0.05, 0.95)} is equivalent to the 5th and 95th percentiles
#' of the marginal posterior density.
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
#' @export parameter.plot
#' @exportMethod  parameter.plot

parameter.plot <- function(mcmclist, ...) UseMethod("parameter.plot", mcmclist)

###############################################################################
###############################################################################
#' @title Graph of marginal posterior densities
#'
#' @description
#' Graph of marginal posterior densities for the item parameters (\emph{a},
#' \emph{b} or \emph{c}).
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' Graph of posterior densities of the item parameter (\emph{a}, \emph{b} or
#' \emph{c}).
#'
#' @details
#' Graph of marginal posterior densities of the item parameter \emph{a, b}
#' for \emph{mcmc.2pnob} object or \emph{a, b, c} for \emph{mcmc.3pnob object}.
#' The center of error bar corresponds to the marginal posterior mean and
#' the extremes correspond to percentiles of the marginal posterior density
#' (These are delimited for \emph{prob}). For example,
#' \emph{prob = c(0.05, 0.95)} is equivalent to the 5th and 95th percentiles
#' of the marginal posterior density.
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

parameter.plot.default <- function(mcmclist, ...) NULL

###############################################################################
###############################################################################
#' @title Graph of marginal posterior densities
#'
#' @description
#' Graph of marginal posterior densities for the item parameters (\emph{a},
#' \emph{b} or \emph{c}).
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param prob A vector of length two for defined the percentiles of the
#' posterior density.
#' @param items A vector to indicate the item to be plotted.
#' @param parameter The parameter (\emph{a}, \emph{b}, \emph{c} or
#' \emph{theta}) for graphing.
#' @param ... Further arguments.
#'
#' @return
#' Graph of posterior densities of the item parameter (\emph{a}, \emph{b} or
#' \emph{c}).
#'
#' @details
#' Graph of marginal posterior densities of the item parameter \emph{a, b}
#' for \emph{mcmc.2pnob} object or \emph{a, b, c} for \emph{mcmc.3pnob object}.
#' The center of error bar corresponds to the marginal posterior mean and
#' the extremes correspond to percentiles of the marginal posterior density
#' (These are delimited for \emph{prob}). For example,
#' \emph{prob = c(0.05, 0.95)} is equivalent to the 5th and 95th percentiles
#' of the marginal posterior density.
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
#' parameter.plot(model2)
#' parameter.plot(model2, items = c(2, 10:15))
#' parameter.plot(model2, items = 1:100, parameter = "theta" )
#'
#' \donttest{
#' # For all examinees of the data
#' # Three-Parameter Normal Ogive Model
#' model3 <- mcmc.3pnob(MathTest, iter = 3500, burning = 500)
#' parameter.plot(model3)
#' parameter.plot(model3, items = c(2, 10:15))
#' parameter.plot(model3, items = 1:100, parameter = c("c", "theta"))
#' }
#'
#' ## End(Not run)
#'
#' @export
#'
#' @importFrom graphics abline boxplot hist legend lines par plot points
#' segments text

parameter.plot.bairt <- function(mcmclist, items = NULL, parameter = NULL,
                                 prob = c(0.05, 0.95), ...) {


  #save old par() ===========================================================

  old <- par(no.readonly = TRUE)

  data <- mcmclist$information$data

  parameterNumber <- table(mcmclist$diagnostic$diag$Parameter)


  if(is.null(parameter)){

    parameter <- names(parameterNumber)[1:(length(parameterNumber) - 1)]

    #
    if(is.null(items)){items <- 1: ncol(data)

    } else {

     items <- items[( items %in% 1:max(parameterNumber[parameter]) )]

    }#

  }else{

    .parameter.test(mcmclist, parameter)

    if(is.null(items)){items <- 1: ncol(data)

    } else {

      items <- items[( items %in% 1:max(parameterNumber[parameter]) )]

    }

  }

  par(mfrow =
        c( length(parameter[parameter %in% names(parameterNumber)] ), 1) )

  for(i in 1:length(parameter[parameter %in% names(parameterNumber)] )){

    if(parameter[i] == "a"){name <- "DISCRIMINATION (a)"}
    if(parameter[i] == "b"){name <- "DIFFICULTY (b)"}
    if(parameter[i] == "c"){name <- "GUESS (c)"}
    if(parameter[i] == "theta"){name <- "ABILITY (theta)"}

    item.study(mcmclist, items, parameter[i], main = name, ...)

  }

  #Return old par() =========================================================

  on.exit(par(old))

  invisible()

}

###############################################################################
###############################################################################
item.study <- function(mcmclist, items, parameter,
                       prob = c(0.05, 0.95), ...) {


  diag <- mcmclist$diagnostic$diag

  con <- paste0(100 * prob, "%") %in% names(diag)

  # Check compute prob ========================================================

  if (con[1] && con[2]) {

    diagA <- diag[diag[, "Parameter"] == parameter,
                  c(paste0(100 * prob, "%"), "Mean")]

  } else {

    mcmcobj <- mcmclist$mcmcobj

    diag <- .diag.mcmcobj(mcmclist$mcmcobj, mcmclist$information$parts, prob)

    diagA <- diag[diag[, "Parameter"] == parameter,
                  c(paste0(100 * prob, "%"), "Mean")]

  }


  Item <- (1:nrow(diagA))[items]

  Value <- (diagA$Mean)[items]

  # Plot ======================================================================

  plot(Item, Value, cex = 0.75, ylim = c(min(diagA), max(diagA)), ...)

  abline(h = 0, lty = 3)


  for (i in items) {

    # Vertical lines ---------------------------------------

    segments(x0 = i, y0 = diagA[i, 1], x1 = i, y1 = diagA[i, 2])

    l <- 0.15

    # Horizontal lines -------------------------------------

    segments(x0 = i - l, y0 = diagA[i, 1], x1 = i + l, y1 = diagA[i, 1])

    segments(x0 = i - l, y0 = diagA[i, 2], x1 = i + l, y1 = diagA[i, 2])

  }

}


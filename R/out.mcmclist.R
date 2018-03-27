#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#complementary functions for mcmclist objects
#
###############################################################################
###############################################################################
#' @title Diagnostic of \emph{mcmc.2pnob} or \emph{mcmc.3pnob} object
#'
#' @description
#' This function gives the summary for all MCMC chains. It including calculus of
#' Rhat, posterior mean, posterior standard deviation and posterior quartiles.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' Data frame with the summary. It including calculus of Rhat, posterior mean,
#' posterior standard deviation and posterior quartiles.
#'
#' @author
#' Javier Martínez
#'
#' @references
#' Gelman, A., Carlin, J. B., Stern, H. S. & Rubin, B. (2004).
#' Bayesian Data Analysis.New York: Chapman & Hall/CRC.
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
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 100, burning = 0)
#' diagnostic.mcmc(model2)
#'
#' \donttest{
#' # For all examinees of the data MathTest
#' # Three-Parameter Normal Ogive Model
#' model3 <- mcmc.3pnob(MathTest, iter = 3500, burning = 500)
#' diagnostic.mcmc(model3)
#' }
#'
#' ## End(Not run)
#'
#'
#' @export

diagnostic.mcmc <- function(mcmclist, ...){

  if ( !("bairt" %in% class(mcmclist) ) ){

    stop(paste0(mcmclist, "is not a bairt class" ), call. = FALSE)

  }

  diag <- mcmclist$diagnostic$diag

  return(diag)

}

###############################################################################
###############################################################################
#' @title Values of the last iteration for each chain
#'
#' @description
#' This function gives the values of the last iteration for each chain. This is
#' useful for assign the initial values from new MCMC models.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' A list with the last values simulated from a \emph{mcmc.2pnob} or
#' \emph{mcmc.3pnob} class object.
#'
#' @author
#' Javier Martínez
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
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 100, burning = 0)
#'
#' # continue the  MCMC for the Two-Parameter Normal Ogive Model
#' initialValues2 <- final.values.mcmc(model2)
#' model121 <- mcmc.2pnob(MathTest[1:500,], initial.value = initialValues2,
#' iter = 100, burning = 0)
#'
#' \donttest{
#' # For all examinees of the data MathTest
#' # Three-Parameter Normal Ogive Model
#' model3 <- mcmc.3pnob(MathTest, iter = 3500, burning = 500)
#'
#' # continue the  MCMC for the Three-Parameter Normal Ogive Model
#' initialValues3 <- final.values.mcmc(model3)
#' model131 <- mcmc.3pnob(MathTest, initial.value = initialValues3,
#' iter = 3000, burning = 0)
#' }
#'
#' ## End(Not run)
#'
#' @export


final.values.mcmc <- function(mcmclist, ...){

  if ( !("bairt" %in% class(mcmclist) ) ){

    stop(paste0(mcmclist, "is not a bairt class" ), call. = FALSE)

  }

  final <- mcmclist$information$final.values

  return(final)

}

###############################################################################
###############################################################################
#' @title Number of Iterations for an MCMC object.
#'
#' @description
#' This function gives the number of Iterations for a class object
#' \emph{mcmc.2pnob} or \emph{mcmc.3pnob}.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' The number of iterations for an \emph{mcmc.2pnob} or \emph{mcmc.3pnob} object.
#'
#' @author
#' Javier Martínez
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
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 100, burning = 0)
#' iter.mcmc(model2)
#' burning.mcmc(model2)
#' thin(model2)
#' parts.mcmc(model2)
#' model.mcmc(model2)
#' data.mcmc(model2)
#'
#' \donttest{
#' # For all examinees of the data MathTest
#' # Three-Parameter Normal Ogive Model
#' model3 <- mcmc.3pnob(MathTest, iter = 3500, burning = 500)
#' iter.mcmc(model3)
#' burning.mcmc(model3)
#' thin(model3)
#' parts.mcmc(model3)
#' model.mcmc(model3)
#' data.mcmc(model3)
#' }
#'
#' ## End(Not run)
#'
#' @export

iter.mcmc <- function(mcmclist, ...){

  if ( !("bairt" %in% class(mcmclist) ) ){

    stop(paste0(mcmclist, "is not a bairt class" ), call. = FALSE)

  }

  iter <- mcmclist$information$iter

  names(iter) <- paste0("iter")

  return(iter)

}

###############################################################################
###############################################################################
#' @title Burning of MCMC objects
#'
#' @description
#' This function gives the number of the first discarded iterations for an MCMC
#' object of class mcmc.2pnob or mcmc.3pnob.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' The burning number for a \emph{mcmc.2pnob} or \emph{mcmc.3pnob} object.
#'
#' @author
#' Javier Martínez
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
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 100, burning = 0)
#' iter.mcmc(model2)
#' burning.mcmc(model2)
#' thin(model2)
#' parts.mcmc(model2)
#' model.mcmc(model2)
#' data.mcmc(model2)
#'
#' \donttest{
#' # For all examinees of the data MathTest
#' # Three-Parameter Normal Ogive Model
#' model3 <- mcmc.3pnob(MathTest, iter = 3500, burning = 500)
#' iter.mcmc(model3)
#' burning.mcmc(model3)
#' thin(model3)
#' parts.mcmc(model3)
#' model.mcmc(model3)
#' data.mcmc(model3)
#' }
#'
#' ## End(Not run)
#'
#' @export

burning.mcmc <- function(mcmclist, ...){

  if ( !("bairt" %in% class(mcmclist) ) ){

    stop(paste0(mcmclist, "is not a bairt class" ), call. = FALSE)

  }

  burning <- mcmclist$information$burning

  names(burning) <- paste0("burning")

  return(burning)

}

#################################################################################
#################################################################################
#' @title Thinning interval
#'
#' @description
#' This function gives the thinning interval for a \emph{mcmc.2pnob}
#' or \emph{mcmc.3pnob} object.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} object class object.
#' @param ... Further arguments.
#'
#' @return
#' The thinning interval for a \emph{mcmc.2pnob} or \emph{mcmc.3pnob} object.
#'
#' @author
#' Javier Martínez
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
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 100, burning = 0)
#' iter.mcmc(model2)
#' burning.mcmc(model2)
#' thin(model2)
#' parts.mcmc(model2)
#' model.mcmc(model2)
#' data.mcmc(model2)
#'
#' \donttest{
#' # For all examinees of the data MathTest
#' # Three-Parameter Normal Ogive Model
#' model3 <- mcmc.3pnob(MathTest, iter = 3500, burning = 500)
#' iter.mcmc(model3)
#' burning.mcmc(model3)
#' thin(model3)
#' parts.mcmc(model3)
#' model.mcmc(model3)
#' data.mcmc(model3)
#' }
#'
#' ## End(Not run)
#'
#' @export

thin <- function(mcmclist, ...){

  if ( !("bairt" %in% class(mcmclist) ) ){

    stop(paste0(mcmclist, "is not a bairt class" ), call. = FALSE)

  }

  thinn <- mcmclist$information$thin

  names(thinn) <- paste0("thin")

  return(thinn)

}


###############################################################################
###############################################################################
#' @title Number of splits for MCMC chain
#'
#' @description
#' This function gives the splits number for a \emph{mcmc.2pnob}
#' or \emph{mcmc.3pnob} object.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' The splits number for a \emph{mcmc.2pnob} or \emph{mcmc.3pnob} object.
#'
#' @author
#' Javier Martínez
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
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 100, burning = 0)
#' iter.mcmc(model2)
#' burning.mcmc(model2)
#' thin(model2)
#' parts.mcmc(model2)
#' model.mcmc(model2)
#' data.mcmc(model2)
#'
#' \donttest{
#' # For all examinees of the data MathTest
#' # Three-Parameter Normal Ogive Model
#' model3 <- mcmc.3pnob(MathTest, iter = 3500, burning = 500)
#' iter.mcmc(model3)
#' burning.mcmc(model3)
#' thin(model3)
#' parts.mcmc(model3)
#' model.mcmc(model3)
#' data.mcmc(model3)
#' }
#'
#' ## End(Not run)
#'
#' @export


parts.mcmc <- function(mcmclist, ...){

  if ( !("bairt" %in% class(mcmclist) ) ){

    stop(paste0(mcmclist, "is not a bairt class" ), call. = FALSE)

  }

  parts <- mcmclist$information$parts

  names(parts) <- paste0("parts")

  return(parts)

}


###############################################################################
###############################################################################
#' @title MCMC object model
#'
#' @description
#' This function gives the model from MCMC object.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' The model from MCMC object.
#'
#' @author
#' Javier Martínez
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
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 100, burning = 0)
#' iter.mcmc(model2)
#' burning.mcmc(model2)
#' thin(model2)
#' parts.mcmc(model2)
#' model.mcmc(model2)
#' data.mcmc(model2)
#'
#' \donttest{
#' # For all examinees of the data MathTest
#' # Three-Parameter Normal Ogive Model
#' model3 <- mcmc.3pnob(MathTest, iter = 3500, burning = 500)
#' iter.mcmc(model3)
#' burning.mcmc(model3)
#' thin(model3)
#' parts.mcmc(model3)
#' model.mcmc(model3)
#' data.mcmc(model3)
#' }
#'
#' ## End(Not run)
#'
#' @export

model.mcmc <- function(mcmclist, ...){

  if ( !("bairt" %in% class(mcmclist) ) ){

    stop(paste0(mcmclist, "is not a bairt class" ), call. = FALSE)

  }

  model <- mcmclist$information$model

  names(model) <- paste0("model")

  return(model)

}

###############################################################################
###############################################################################
#' @title MCMC object data
#'
#' @description
#' This function gives the data for an MCMC object.
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' The data for an MCMC object.
#'
#' @author
#' Javier Martínez
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
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 100, burning = 0)
#' iter.mcmc(model2)
#' burning.mcmc(model2)
#' thin(model2)
#' parts.mcmc(model2)
#' model.mcmc(model2)
#' data.mcmc(model2)
#'
#' \donttest{
#' # For all examinees of the data MathTest
#' # Three-Parameter Normal Ogive Model
#' model3 <- mcmc.3pnob(MathTest, iter = 3500, burning = 500)
#' iter.mcmc(model3)
#' burning.mcmc(model3)
#' thin(model3)
#' parts.mcmc(model3)
#' model.mcmc(model3)
#' data.mcmc(model3)
#' }
#'
#' ## End(Not run)
#'
#' @export

data.mcmc <- function(mcmclist, ...){

  if ( !("bairt" %in% class(mcmclist) ) ){

    stop(paste0(mcmclist, "is not a bairt class" ), call. = FALSE)

  }

  data <- mcmclist$information$data

  return(data)

}

#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#select.c.prior
#
###############################################################################
###############################################################################
#' @title Select the c prior for the Three-Parameter Normal Ogive Model
#'
#' @description
#' Select the \emph{c} (guessing parameter) prior for \emph{mcmc.3pnob},
#' through the application of Bayes Modal Estimation Equations.
#'
#' @usage
#' select.c.prior(nitem, m = 20, ...)
#'
#' @param nitem  Number of alternatives for each item.
#' @param m It is a priori weight assigned to the prior information.
#' \eqn{m = 20} by default.
#' @param ... Further arguments.
#'
#' @return
#' A vector length 2, this indicate the \emph{c} (guessing parameter) prior for
#' \emph{mcmc.3pnob}.
#'
#' @details
#' Because \emph{c} (guessing parameter) is bounded by 0 and 1, a
#' \eqn{Beta(\alpha, \beta)} prior distribution was proposed by Swaminathan and
#' Gifford (1986). These parameters are defined as \eqn{\alpha=mp+1} and
#' \eqn{\beta=m(p-1)+1}, where \eqn{p=1/n} with \emph{n} = number of
#' alternatives for each item  (Harwell & Baker, 1991, p.386)
#'
#' @author
#' Javier Martínez
#'
#' @references
#' Harwell, M. R, & Baker, F. B. (1991). The use of Prior Distributions in
#' Marginalized Bayesian Item Parameter Estimation: A Didactic. Psychometrika,
#' 15, 375-389.
#'
#' @seealso
#' \code{\link{mcmc.3pnob}} and \code{\link{continue.mcmc.bairt}}.
#'
#' @examples
#' # data for model
#' data("MathTest")
#'
#' # selection of the prior for 5 response options
#' cprior <- select.c.prior(5)
#'
#' # estimate model only for the first 500 examinees of the data MathTest
#' model3 <- mcmc.3pnob(MathTest[1:500,], iter = 300, burning = 0,
#'                     c.prior = cprior)
#'
#' ## End(Not run)
#'
#' @export

select.c.prior <- function(nitem, m = 20, ...){

    if( nitem == 0 ){

    prior <- c( 1, 1) } else {

      alpha <- ( m *( 1 / nitem ) ) + 1

      alpha <- round( alpha, digits = 2 )

      beta <- ( m * ( 1 - ( 1 / nitem ) ) ) + 1

      beta <- round( beta, digits = 2)

      prior <- c( alpha, beta  )

    }

  return( prior )

  }



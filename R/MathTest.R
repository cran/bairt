#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#MathTest data
#
###############################################################################
###############################################################################
#' @name MathTest
#'
#' @title Data from an math test applied at USB in 2012.
#'
#' @description Observed data for a math test designed by Simon
#' Bolivar University (USB) in 2012. It is represented by an 100 × 15
#' matrix of 1’s and 0’s.
#'
#' @docType data
#'
#' @usage data(MathTest)
#'
#' @keywords datasets
#'
#' @format The \emph{i-th} row of this matrix represents the answers from the
#' \emph{i-th} examinee, whereas the elements in \emph{j-th} column represents
#' the answers from the examinees to the \emph{j-th} test item.
#'
#' @examples
#' # data for model
#' data("MathTest")
#'
#' # Only for the first 500 examinees of the data MathTest
#' # Two-Parameter Normal Ogive Model
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 400, burning = 100)
#' check.plot(model2)
#'
#' \donttest{
#' # For all examinees of the data MathTest
#' # Three-Parameter Normal Ogive Model
#' # selection of the prior for 5 response options
#' cprior <- select.c.prior(5)
#' model3 <- mcmc.3pnob(MathTest, iter = 3500, burning = 500,
#'                     c.prior = cprior)
#' check.plot(model3)
#' irc(model3, item = 11)
#' chain.study(model3, chain = 11, parameter = "a")
#' parameter.plot(model3)
#' }
#'
#' ## End(Not run)
NULL

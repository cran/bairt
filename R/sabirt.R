#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#sabirt
#
###############################################################################
###############################################################################
#' @title Shiny App for Bayesian Item Response Theory (SABIRT)
#'
#' @description
#' A web interactive application intended for the making of an MCMC (Markov
#' Chain Monte Carlo Methods) estimation and model-fit of the item response
#' models designed by Johnson and Albert (2pno, 1999) and Glas and Beguim
#' (3pno, 2001). The outcome are the items parameters (difficulties and
#' discrimination for 2pno, and additionally the chance to guess the right
#' answers for 3pno) and also the latent abilities of each examinee.
#'
#' @usage
#' sabirt()
#'
#' @author
#' Javier Martínez and Irene Garcia Mosquera
#'
#' @references
#' Beguin, A, A. & Glas, C.A.W. (2001). MCMC Estimation and Some Model-Fit
#' Analysis of Multidimensional IRT Models. Psychometrika, 66, 541-562.
#'
#' Harwell, M. R, & Baker, F. B. (1991). The use of Prior Distributions in
#' Marginalized Bayesian Item Parameter Estimation: A Didactic. Psychometrika,
#' 15, 375-389.
#'
#' Johnson, V. E. & Albert, J. H. (1999). Ordinal Data Modeling.
#' New York: Springer.
#'
#' @export  sabirt
#' @import shiny

sabirt <- function(){

 shinyjs::useShinyjs()

 shiny::runApp(system.file("sabirt", package = "bairt"))

}

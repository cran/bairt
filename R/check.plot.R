#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#check.plot
#
###############################################################################
###############################################################################
#' @title Plot of the discrimination marginal posterior means against
#' difficulty marginal posterior means
#'
#' @description
#' Marginal Posterior means of \eqn{b_j} plotted against the
#' marginal posterior means of \eqn{a_j}. Each point is labeled with
#' the number of the corresponding Item.
#'
#' For the Three-Parameter Normal Ogive Item Response Model \emph{(3pno)},
#' the size of the numbers refers to the marginal posterior means of
#' \eqn{c_j}.
#'
#' The Potential Scale Reduction Factor (\emph{Rhat}) is calculated
#' for each chain, \emph{bairt} generates a single MCMC and evaluates
#' convergence by breaking the chain in three sub chains and comparing the
#' between- and within-subchain variance.
#'
#' The \emph{black color suggests convergence} and
#' \emph{red items indicate convergence problems} (\emph{Rhat greater than 1.1}).
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' A plot of the discrimination marginal posterior means against
#' difficulty marginal posterior means. For the Three-parameter model
#' the guessing marginal posterior means are represented by the
#' number size of the item.
#'
#' @details
#' If \emph{converg.test = TRUE} the items with Rhat menor that 1.1 are print
#' in red color. It is useful for quick check of the convergence.
#'
#' @author
#' Javier Martínez
#'
#' @references
#' Johnson, V. E. & Albert, J. H. (1999). Ordinal Data Modeling.
#' New York: Springer.
#'
#' Gelman, A., Carlin, J. B., Stern, H. S. & Rubin, B. (2004).
#' Bayesian Data Analysis.New York: Chapman & Hall/CRC.
#'
#' @seealso
#' \code{\link{mcmc.2pnob}}, \code{\link{mcmc.3pnob}} and
#' \code{\link{continue.mcmc.bairt}}.
#'
#' @export check.plot
#' @exportMethod check.plot
#'

check.plot <- function(mcmclist, ...) UseMethod("check.plot", mcmclist)

###############################################################################
###############################################################################
#' @title Plot of the discrimination marginal posterior means against
#' difficulty marginal posterior means
#'
#' @description
#' Marginal Posterior means of \eqn{b_j} plotted against the
#' marginal posterior means of \eqn{a_j}. Each point is labeled with
#' the number of the corresponding Item.
#'
#' For the Three-Parameter Normal Ogive Item Response Model \emph{(3pno)},
#' the size of the numbers refers to the marginal posterior means of
#' \eqn{c_j}.
#'
#' The Potential Scale Reduction Factor (\emph{Rhat}) is calculated
#' for each chain, \emph{bairt} generates a single MCMC and evaluates
#' convergence by breaking the chain in three sub chains and comparing the
#' between- and within-subchain variance.
#'
#' The \emph{black color suggests convergence} and
#' \emph{red items indicate convergence problems} (\emph{Rhat greater than 1.1}).
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param ... Further arguments.
#'
#' @return
#' A plot of the discrimination marginal posterior means against
#' difficulty marginal posterior means. For the Three-parameter model
#' the guessing marginal posterior means are represented by the
#' number size of the item.
#'
#' @details
#' If \emph{converg.test = TRUE} the items with Rhat menor that 1.1 are print
#' in red color. It is useful for quick check of the convergence.
#'
#' @author
#' Javier Martínez
#'
#' @references
#' Johnson, V. E. & Albert, J. H. (1999). Ordinal Data Modeling.
#' New York: Springer.
#'
#' Gelman, A., Carlin, J. B., Stern, H. S. & Rubin, B. (2004).
#' Bayesian Data Analysis.New York: Chapman & Hall/CRC.
#'
#' @seealso
#' \code{\link{mcmc.2pnob}}, \code{\link{mcmc.3pnob}} and
#' \code{\link{continue.mcmc.bairt}}.
#'
#' @export
#'

check.plot.default <- function(mcmclist, ...) {

  print(paste0(mcmclist, "is not a bairt class"), ...)

}

###############################################################################
###############################################################################
#' @title Plot of the discrimination marginal posterior means against
#' difficulty marginal posterior means
#'
#' @description
#' Marginal Posterior means of \eqn{b_j} plotted against the
#' marginal posterior means of \eqn{a_j}. Each point is labeled with
#' the number of the corresponding Item.
#'
#' For the Three-Parameter Normal Ogive Item Response Model \emph{(3pno)},
#' the size of the numbers refers to the marginal posterior means of
#' \eqn{c_j}.
#'
#' The Potential Scale Reduction Factor (\emph{Rhat}) is calculated
#' for each chain, \emph{bairt} generates a single MCMC and evaluates
#' convergence by breaking the chain in three sub chains and comparing the
#' between- and within-subchain variance.
#'
#' The \emph{black color suggests convergence} and
#' \emph{red items indicate convergence problems} (\emph{Rhat greater than 1.1}).
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param converg.test  Checking if Rhat is major that 1.1.
#' @param ... Further arguments.
#'
#' @return
#' A plot of the discrimination marginal posterior means against
#' difficulty marginal posterior means. For the Three-parameter model
#' the guessing marginal posterior means are represented by the
#' number size of the item.
#'
#' @details
#' If \emph{converg.test = TRUE} the items with Rhat menor that 1.1 are print
#' in red color. It is useful for quick check of the convergence.
#'
#' @author
#' Javier Martínez
#'
#' @references
#' Johnson, V. E. & Albert, J. H. (1999). Ordinal Data Modeling.
#' New York: Springer.
#'
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
#' @export
#'
#' @importFrom graphics abline boxplot hist legend lines par plot points
#' segments text


check.plot.mcmc.2pnob <- function(mcmclist, converg.test = TRUE, ...) {


  diag <- mcmclist$diagnostic$diag

  para <- split(diag$Mean, diag$Parameter)

  a <- para$a

  b <- para$b

  abc <- c(a, b)

  l <- length(abc)

  R <- split(diag$Rhat, diag$Parameter)[1:2]

  # Test convergence ==========================================================

  Rc <- unlist(lapply(R, .major.fun))

  color <- rep(1, l)

  if (converg.test) {

    color[Rc] = 2

  }

  m <- cbind(1:length(a), a, b, color)

  # xlim and ylim =============================================================

  am <- c(min(a) - 1, max(a) + 1)

  bm <- c(min(b) - 2, max(b) + 2)

  # Plot ======================================================================

  plot(b, a, type = "n", xlim = bm, ylim = am,
       xlab = "Posterior Mean Difficulty (b)",
       ylab = "Posterior Mean Discrimination (a)", ...)

  # Item numbers -------------------------------------------

  for (i in 1:length(a)) {

    text(m[i, 3], m[i, 2], labels = round(m[i, 1], 1), pos = 1,
         offset = 1, cex = 1, col = m[i, 4])

  }


  # legend -------------------------------------------------

  if (converg.test) {

    legend("topright", c("Convergence is OK", "Convergence not reached"),
           pch = 0, bty = "n", col = c(0, 0), text.col = c(1, 2))

  }

}

###############################################################################
###############################################################################
#' @title Plot of the discrimination marginal posterior means against
#' difficulty marginal posterior means
#'
#' @description
#' Marginal Posterior means of \eqn{b_j} plotted against the
#' marginal posterior means of \eqn{a_j}. Each point is labeled with
#' the number of the corresponding Item.
#'
#' For the Three-Parameter Normal Ogive Item Response Model \emph{(3pno)},
#' the size of the numbers refers to the marginal posterior means of
#' \eqn{c_j}.
#'
#' The Potential Scale Reduction Factor (\emph{Rhat}) is calculated
#' for each chain, \emph{bairt} generates a single MCMC and evaluates
#' convergence by breaking the chain in three sub chains and comparing the
#' between- and within-subchain variance.
#'
#' The \emph{black color suggests convergence} and
#' \emph{red items indicate convergence problems} (\emph{Rhat greater than 1.1}).
#'
#' @param mcmclist A \emph{mcmc.2pnob} or \emph{mcmc.3pnob} class object.
#' @param converg.test  Checking if Rhat is major that 1.1.
#' @param c.probs Vector for assignment of intervals the Guessing (\emph{c}).
#' @param legen Coordinates to be used to position the Guessing
#' (\emph{c}) legend.
#' @param ... Further arguments.
#'
#' @return
#' A plot of the discrimination marginal posterior means against
#' difficulty marginal posterior means. For the Three-parameter model
#' the guessing marginal posterior means are represented by the
#' number size of the item.
#'
#' @details
#' If \emph{converg.test = TRUE} the items with Rhat menor that 1.1 are print
#' in red color. It is useful for quick check of the convergence.
#'
#' @author
#' Javier Martínez
#'
#' @references
#' Johnson, V. E. & Albert, J. H. (1999). Ordinal Data Modeling.
#' New York: Springer.
#'
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
#' model2 <- mcmc.2pnob(MathTest[1:500,], iter = 400, burning = 100)
#' check.plot(model2)
#' chain.study(model2, parameter = "b", chain = 12)
#' chain.study(model2, parameter = "theta", chain = 10)
#'
#' \donttest{
#' # For all examinees of the data
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
#' @export
#'
#' @importFrom graphics abline boxplot hist legend lines par plot points
#' segments text

check.plot.mcmc.3pnob <- function(mcmclist, converg.test = T,
                            c.probs = c(0, 0.2, 0.5, 1),
                            legen = "topleft", ...) {

  # Check in ==================================================================

  .c.probs.test(c.probs)

  # Initial Values ============================================================

  diag <- mcmclist$diagnostic$diag

  para <- split(diag$Mean, diag$Parameter)

  a <- para$a

  b <- para$b

  c <- para$c

  abc <- c(a, b, c)

  l <- length(abc)

  R <- split(diag$Rhat, diag$Parameter)[1:3]

  # Test convergence ==========================================================

  Rc <- unlist(lapply(R, .major.fun))

  color <- rep(1, l)

  if (converg.test) {

    color[Rc] = 2

  }

  # c scale ===================================================================

  c <- .scalec(c, c.probs)

  m <- cbind(1:length(a), a, b, c, color)

  am <- c(min(a) - 2, max(a) + 2)

  bm <- c(min(b) - 5, max(b) + 4)

  # Plot ======================================================================

  plot(b, a, type = "n", xlim = bm, ylim = am,
       xlab = "Posterior Mean Difficulty (b)",
       ylab = "Posterior Mean Discrimination (a)", ...)

  # Item numbers -------------------------------------------

  for (i in 1:length(a)) {

    text(m[i, 3], m[i, 2], labels = round(m[i, 1], 1), pos = 1, offset = 1,
         cex = m[i, 4], col = m[i, 5])

  }

  # legend -------------------------------------------------

  legend(legen, matrix(.int(c.probs), ncol = 1),
         title = "Posterior Mean Guessing (c)",
         bty = "n", pt.cex = 1:3, pch = "c")


  if (converg.test) {

    legend("topright", c("Convergence is OK", "Convergence not reached"),
           pch = 0, bty = "n", col = c(0, 0), text.col = c(1, 2))

  }


}


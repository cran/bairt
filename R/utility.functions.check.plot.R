#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#utility functions for check.plot
#
###############################################################################
###############################################################################
.major.fun <- function(x, major = 1.1) {


    a <- rep(NA, length(x))

    for (i in 1:length(x)) {

        if (x[i] > major) {
            a[i] = i
        }

    }

    return(a[!is.na(a)])

}

###############################################################################
###############################################################################
.scalec <- function(x, c = seq(0, 1, 0.25)) {


    v <- rep(0, length(x))

    h <- length(c) - 1

    for (j in 1:h) {

        for (i in 1:length(x)) {

            if (c[j] <= x[i] && x[i] < c[j + 1])
                (v[i] = j)
        }

    }

    return(v)

}

###############################################################################
###############################################################################
.int <- function(x) {

    a <- rep(NA, length(x) - 1)

    for (i in 1:(length(x) - 1)) {

        a[i] <- paste("[", x[i], ",", x[i + 1], ")")

    }

    return(a)

}

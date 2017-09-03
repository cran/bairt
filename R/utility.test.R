#BAIRT
#Bayesian Analysis of Item Response Theory Models
#Autor: Javier Martinez <martinezjavier243@gmail.com>
#
#utility functions for initial tests
#
###############################################################################
###############################################################################
.c.probs.test <- function(c.probs, ...){


  if( length(c.probs) != 4 ){

    stop(paste0( c.probs, "is not length 4" ), call. = FALSE)
  }

  for ( i in 1:length(c.probs)){

    if(c.probs[i] > 1 && c.probs[i] < 0 ){

      stop(paste0(c.probs, "is not a probability" ), call. = FALSE)

    }
  }

}

###############################################################################
###############################################################################
.test <- function( iter, burning, thin, parts ){

  n <- ( iter - burning ) / thin


  if( !.whole( iter ) || !.whole( burning )
      || !.whole( thin ) || !.whole( parts ) ){

    stop(paste0( "iter, burning, thin, parts not integer values" ),
          call. = FALSE)

  }

  if( is.na( iter ) || is.na( burning )
       || is.na( thin ) ){

    stop( paste0( "iter, burning, thin whit NA values" ), call. = FALSE)

  }

  if( iter == Inf || burning == Inf || thin == Inf ){

    stop( paste0( "iter, burning, thin whit Inf" ), call. = FALSE)

  }

  if( iter == -Inf || burning == -Inf || thin == -Inf  ){

    stop( paste0( "iter, burning, thin whit -Inf" ), call. = FALSE)

  }

  if( burning >= iter ){

    stop( paste0( "burning >= iter" ), call. = FALSE)

  }

  if( iter < 0 || burning < 0 || thin < 0 ){

    stop( paste0( "iter, burning, thin whit negative values" ), call. = FALSE)

  }

  if( iter == 0 || thin == 0 ){

    stop( paste0( "iter, thin == 0" ), call. = FALSE)

  }

  if( !.whole(n) ){

    stop( paste0( "( ( iter - burnin )/ thin ) is not a integer number" ),
         call. = FALSE)

  }

  if( parts <= 0 || is.na( parts ) ||
      parts == Inf || parts == -Inf ) {

    stop( paste0( "parts","=", parts), call. = FALSE )

  }

  if( (trunc( n ) / parts) < 2 || is.na( trunc( n ) / parts )
      || (trunc( n ) / parts) == Inf || (trunc( n ) / parts) == -Inf ){

    stop( paste0( "parts", "=", parts), call. = FALSE )

  }

  }

###############################################################################
###############################################################################
.whole <- function(x){

  ent <- x - trunc( x )

  if( ent == 0 && x >= 0){ return( TRUE ) } else {

    return( FALSE )

  }
}

###############################################################################
###############################################################################
.item.test <- function (mcmclist, item) {


  if ( !( item %in% 1:(ncol( mcmclist$information$data )) ) ){

    stop( paste0( "item is incorrect" ), call. = FALSE)

  }

}

###############################################################################
###############################################################################
.chain.test <- function ( mcmclist, parameter, chain ) {

  nchain <- ncol( mcmclist$mcmcobj[[ parameter]] )

  if ( !( chain %in% 1:nchain ) ){

    stop( paste0( "chain is incorrect" ), call. = FALSE)

  }
}

###############################################################################
###############################################################################
.parameter.test <- function ( mcmclist, parameter ){

  model <- mcmclist$information$model

  if ( ("mcmc.2pnob" %in% class(mcmclist))
      && !( parameter %in% c( "a", "b", "theta" ) ) ){

    stop( paste0( parameter," is not a parameter" ), call. = FALSE)

  }

  if ( ("mcmc.3pnob" %in% class(mcmclist))
      && !( parameter %in% c( "a", "b", "c", "theta" ) ) ){

    stop( paste0( parameter," is not a parameter" ), call. = FALSE)

  }

}

###############################################################################
###############################################################################
.initial.value.test <- function ( data, initial.value ){

  data <- as.matrix( data )

  nexa <- nrow ( data )

  nitem <- ncol ( data )

  name <- names( initial.value )

  if ( is.null( initial.value )){

    initial.value <- list()

  }


  if ( !is.list( initial.value )){

    stop( paste0( "initial.value is not a list" ), call. = FALSE)

  }

  if ( !( length( initial.value ) %in% c(0, 3, 4 ) ) ){

    stop( paste0( "initial.value is not defined" ), call. = FALSE)

  }


  if ( length( initial.value ) == 3 ){

    if ( !("a" %in% name) && !("b" %in% name) &&
         !("theta" %in% name)){

      stop( paste0( "Parameter name is not defined in initial.value" ),
        call. = FALSE)

    }

    if ( length( initial.value$a ) != nitem
         || length( initial.value$b ) != nitem
         || length( initial.value$theta ) != nexa ){

      stop( paste0( "initial.value is not defined" ),
            call. = FALSE)

    }

  }


    if ( length( initial.value ) == 4 ){

      if ( !("a" %in% name) && !("b" %in% name) &&
          !("c" %in% name) && !("theta" %in% name) ){

        stop( paste0( "Parameter name is not defined" ), call. = FALSE)

      }

      if ( length( initial.value$a ) != nitem
           || length( initial.value$b ) != nitem
           || length( initial.value$c ) != nitem
           || length( initial.value$theta ) != nexa ){

        stop( paste0( "initial.value is not defined" ),
              call. = FALSE)

      }

  }

}

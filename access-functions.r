# calculate accessibility given weights, a function, and a travel times matrix
access <- function( times_odt, weights_o, weights_d, acc_func ){
	# assert that things are the right size
	stopifnot( dim(times_odt)[1] == length(weights_d) )
	stopifnot( dim(times_odt)[1] == dim(times_odt)[2] )
	stopifnot( length(weights_o) == length(weights_d) )
	# need to permute array in order to use vector recycling on weights
	times_dot = aperm( times_odt, c(2,1,3) )
	# d,o,t -> o,t by summing over destinations
	acc_ot = apply( 
		acc_func(times_dot) * weights_d, 
		c(2,3), sum, na.rm=T 
	) / sum(weights_d)
	# o,t -> o by averaging over times (all times given same weight)
	acc_o = apply( acc_ot, c(1), mean, na.rm=T )
	# total summary measure with o weights (o weights get recycled)
	acc = sum( acc_ot * weights_o ) / sum( weights_o * dim(acc_ot)[2] ) 
	return(list( acc, acc_o, acc_ot ))
}


# --------- define access metric functions -------------- 
# cumulative binary (45 mins)
cum <- function(t){ return( t/60L <= 45L ) }
# negative exponential (e^t/30 with t in mins) 
negexp <- function(t){ return( exp(-t/(30*60)) ) }
# gaussian (bw = 30 mins)
gauss <- function(t){ bw = 30*60; return( exp(-(t**2 / ( 2 * bw**2 ) )) ) }

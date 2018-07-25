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
# cumulative binary functions
bin45 <- function(t){ return( t/60L <= 45L ) }
bin60 <- function(t){ return( t/60L <= 60L ) }
bin90 <- function(t){ return( t/60L <= 90L ) }
# gaussian decay functions
gauss30 <- function(t){ 
	bandwidth = 30*60
	return( exp(-(t**2 / (2*bandwidth**2))) ) 
}
gauss45 <- function(t){ 
	bandwidth = 45*60
	return( exp(-(t**2 / (2*bandwidth**2))) ) 
}
gauss60 <- function(t){ 
	bandwidth = 60*60
	return( exp(-(t**2 / (2*bandwidth**2))) ) 
}


A_results = array(NA,c(6,3,2))
dimnames(A_results)[[1]] = c('bin45','bin60','bin90','gauss30','gauss45','gauss60') # function names
dimnames(A_results)[[2]] = c('cor_ot','cor_o','retro/sched') # correlation measures
dimnames(A_results)[[3]] = c('workforce/jobs','zones/zones') # weighting schemes

for( weighting_scheme in dimnames(A_results)[[3]] ){
	print(weighting_scheme)
	w_names = strsplit(weighting_scheme,'/')[[1]]
	w1 = w_names[1]
	w2 = w_names[2]
	# for each of these access functions
	for( func_name in dimnames(A_results)[[1]] ){
		print( func_name )
		func = get(func_name)
		# do the accessibility calculations
		s_acc = access( s_odt, od@data[,w1], od@data[,w2], func )
		r_acc = access( r_odt, od@data[,w1], od@data[,w2], func )
		# pull out the measures and put them in the result matrix
		A_results[func_name,'cor_ot',weighting_scheme] =  cor( c(s_acc[[3]]), c(r_acc[[3]]) )
		A_results[func_name,'cor_o',weighting_scheme] = cor( s_acc[[2]], r_acc[[2]] )
		A_results[func_name,'retro/sched',weighting_scheme] = r_acc[[1]] / s_acc[[1]] 
	}
}
remove(func,func_name,weighting_scheme,w1,w2,w_names)


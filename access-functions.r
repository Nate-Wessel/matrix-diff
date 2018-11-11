# calculate accessibility given weights, a function, and a travel times matrix
access <- function( times_odt, acc_func ){
	# assert that things are the right size
	stopifnot( dim(times_odt)[1] == dim(times_odt)[2] )
	# d,o,t -> o,t by summing over destination
	acc_ot = apply( acc_func(times_odt), c(2,3), sum, na.rm=T ) / dim(times_odt)[1]
	# o,t -> o by averaging over times (all times given same weight)
	acc_o = apply( acc_ot, c(1), mean, na.rm=T )
	# total summary measure with o weights (o weights get recycled)
	acc = mean( acc_ot ) 
	# return all
	return(list( acc, acc_o, acc_ot ))
}

# --------- define access metric functions -------------- 
# cumulative binary (45 mins)
cum <- function(t){ return( t/60L <= 45L ) }
# negative exponential (e^t/30 with t in mins) 
negexp <- function(t){ return( exp(-t/(30L*60L)) ) }
# gaussian (bw = 30 mins)
#gauss <- function(t){ bw = 30*60; return( exp(-(t**2 / ( 2 * bw**2 ) )) ) }

# plot the access functions
if(FALSE){
	pdf(paste0(figures_dir,'A-funcs.pdf'),width=5,height=3)
		par(mar=c(4,2,3,1),family='serif')
		plot( 
			0, type='n', ylim=c(0,1), xlim=c(0,120), bty='n', xaxt="n", yaxt="n",
			main='Accessibility Functions',xlab=NA, ylab=NA
		)
		x = 0:(120*60)
		lines(x=x/60,y=cum(x),col='darkgreen')
		lines(x=x/60,y=negexp(x),col='blue')
		axis( 2, at=c(0,1), labels=c(0,1), las=2, pos=-5 ) # left
		axis( 1, at=c(0,30,60,90,120), labels=c('0','30 minutes','1 hour','1.5 hours','2 hours'), las=0, pos=-.1 ) # bottom
		text(x=c(60,15),y=c(.6,.3),labels=c('Cumulative','Negative\nExponential'),col=c('darkgreen','blue'))
	dev.off()
}
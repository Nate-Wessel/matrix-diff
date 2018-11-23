# load the data
agency = 'Muni'
load(paste0('~/dissdata/R/',agency,'_A.RData'))
gc()

l2 = function(x){log(x,2)} # log base 2
nada = function(x){return(x)} # function that does nothing, used for unlisting lists

# range of parameters in plot
params = seq(0,60,5)
ps = c(-30,-25,-20,-15,-10,-5,0,+5,+10)

# empty plot
#par( mfrow=c(2,2), mar=c(2,4,.5,.4),family='serif') # bottom, left, top, right
plot( 0, type='n', 
	xlim=c(0,60), ylim=l2(range(1+ps/100)), 
	xaxt="n", yaxt="n", xlab='', ylab='', bty='n' 
)
# grid lines
abline( h=l2(1+ps/100), v=c(0,30,60), col=rgb(0,0,0,alpha=0.1))
# y axis
axis( 2, at=l2(1+ps/100), labels=sprintf('%+.0f%%',ps), las=2 )
# x axis
axis( 1, at=c(0,30,60), labels=c('0','30 minutes','1 hour'), las=0 )
# agency name
text(x=0,y=l2(.77),labels=agency,pos=4,cex=2)
# actual lines
for(func in c('cum','negexp')){
	color = switch(func,'cum'='red','negexp'='blue' )
	eAh = sapply(A['retro',func,,'A_oh'],nada) / sapply(A['sched',func,,'A_oh'],nada)
	# quartile range
	ebars = cbind( c(1,1,1), apply(eAh,2,quantile,probs=c(.25,.5,.75),na.rm=T ) )
	# plot the median
	lines( x=params, y=l2(ebars[2,]), type='b',col=color )
	# and the quartile range
	polygon(
		x=c(params,rev(params)), 
		y=l2( c( ebars[1,],rev(ebars[3,]) ) ),
		border=NA, col=adjustcolor(color,alpha=.2)
	)
}
remove(eAh,ebars,ps,color)

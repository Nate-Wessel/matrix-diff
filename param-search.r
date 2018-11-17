# load the data
agency = 'TTC'
load(paste0('~/',agency,'.RData'))

# log base 2
l2 = function(x){log(x,2)}

params = seq(0,60,5)
p = as.character(params[2:13])
eA_neg = c('0'=1, unlist(A['retro','negexp',p,'A']) / unlist(A['sched','negexp',p,'A']) )
eA_cum = c('0'=1, unlist(A['retro','cum',p,'A']) / unlist(A['sched','cum',p,'A']) )

# empty plot
par( mfrow=c(2,2), mar=c(2,4,.5,.4),family='serif') # bottom, left, top, right
ps = c(-20,-15,-10,-5,0)
plot( 0, type='n', 
	xlim=c(range(params)), ylim=l2(c(.8,1.01)), 
	xaxt="n", yaxt="n", xlab='', ylab='', bty='n' 
)
# grid lines
abline( h=l2(1+ps/100), v=c(0,30,60), col=rgb(0,0,0,alpha=0.1))
# y axis
axis( 2, at=l2(1+ps/100), labels=sprintf('%+.0f%%',ps), las=2 )
# x axis
axis( 1, at=c(0,30,60), labels=c('0','30 minutes','1 hour'), las=0 )
# agency name
text(x=0,y=l2(.81),labels=agency,pos=4,cex=2)
# actual lines
lines( x = params, y = l2(eA_cum), type='b' )
lines( x = params, y = l2(eA_neg), type='b', col='red' )

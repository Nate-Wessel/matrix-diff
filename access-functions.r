# --------- define access metric functions -------------- 
# cumulative binary
cum <- function(t,theta){ return( t <= 60L*theta ) }
# negative exponential
negexp <- function(t,beta){ return( exp(-t/(beta*60L)) ) }
# gaussian (bw = 30 mins)
#gauss <- function(t){ bw = 30*60; return( exp(-(t**2 / ( 2 * bw**2 ) )) ) }

# plot the access functions
if(FALSE){
	figures_dir = '/home/nate/Dropbox/diss/paper/figures/'
	pdf(paste0(figures_dir,'A-funcs.pdf'),width=5,height=2.2)
		par(mar=c(3,2,2,1),family='serif')
		plot( 
			0, type='n', ylim=c(0,1), xlim=c(0,120), bty='n', xaxt="n", yaxt="n",
			main='Impedance Functions',xlab=NA, ylab=NA
		)
		x = 0:(120*60)
		lines(x=x/60,y=cum(x,45),col='darkgreen')
		lines(x=x/60,y=negexp(x,30),col='blue')
		axis( 2, at=c(0,1), labels=c(0,1), las=2, pos=-5 ) # left
		axis( 1, at=c(0,30,60,90,120), labels=c('0','30 minutes','1 hour','1.5 hours','2 hours'), las=0, pos=-.1 ) # bottom
		text(x=c(60,15),y=c(.6,.25),labels=c('Cumulative','Negative\nExponential'),col=c('darkgreen','blue'))
	dev.off()
}
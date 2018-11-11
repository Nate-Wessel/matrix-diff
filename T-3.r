agency = 'Muni'
load(paste0('~/',agency,'.RData'))

library('ks')
library('latex2exp')

s_odt[s_odt==0] = 1
r_odt[r_odt==0] = 1


figures_dir = '/home/nate/Dropbox/diss/paper/figures/'

#
# PLOT THE T_odt FIGURES
#
# limit the plot range per agency
hmax = switch( agency, 'Muni'=1.3, 'MBTA'=4, 'TTC'=2.8, 'JTA'=4 ) 
l2 = function(x){log(x,2)} # log base 2
# sample 10k non-null value pairs
s = sample(length(s_odt),200000)
s = s[ !is.na(s_odt[s]) & !is.na(r_odt[s]) ]
s = s[1:100000]
# do the KDE
kde_result = kde( 
	cbind(
		x=s_odt[s]/3600,
		y=l2(r_odt[s]/s_odt[s])
	),
	xmin=c(-0.1,-1.1),xmax=c(hmax+.1,1.1)
)
# define contours
contours = c(10,25,50,75,90)
# plot the schedule travel times against error (sampled points)
pdf(paste0(figures_dir,agency,'-time3.pdf'),width=5.5,height=5.5)
	par( mai=c(0.8,0.75,0.2,0.75), pch='+', family='serif' ) # bottom, left, top, right
	# slice, persp, image, filled.contour
	plot( 
		kde_result, display='filled.contour', cont = contours, #col=heat.colors(6),
		bty='n', xaxt="n", yaxt="n", xlab='', ylab='',
		xlim=c(0,hmax),ylim=c(-1,1)
	)
	# add the axes: hours scheduled  and percent difference
	axis( 1, at=c(0:hmax), labels=paste(0:hmax,'h'), las=0, pos=-1.1 )
	ps = c(.5,.6,.70,.8,.9,1,1.15,1.3,1.5,1.75,2)
	axis( 2, at=l2(ps), labels=sprintf('%+.0f%%',(ps-1)*100), las=2, pos=-hmax/20 )
	title(xlab="Scheduled Travel Time", line=2.5)
	# add absolute difference lines
	x = seq(0,hmax+1,by=1/60)
	hdiffs = c(1,.5,.25,0,-.25,-.5,-1)
	for(hd in hdiffs){ lines( x=x, y=l2((x+hd)/x),lty=2 ) }
	axis( 4, at=l2((hmax+.1+hdiffs)/(hmax+.1)), labels=sprintf('%+.0fm',hdiffs*60),las=2)
	# grid lines
	abline(h=l2(ps),v=0:hmax,col=rgb(0,0,0,alpha=0.1)) # grey grid
	# add contours
	plot( kde_result, display='slice', cont=contours, add=T )
	# add agency name in lower left corner
	text(x=0,y=-0.95,labels=agency,pos=4,cex=2)
dev.off()

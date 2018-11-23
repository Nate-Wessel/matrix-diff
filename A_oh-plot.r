library('ks')
figures_dir = '/home/nate/Dropbox/diss/paper/figures/'
# log base 2
l2 = function(x){log(x,2)} 
# define contours
contours = c(10,25,50,75,90)
#
### PLOT THE A_oh FIGURES
for( agency in c('TTC','JTA','MBTA','Muni')){
	print(agency)
	# get the data
	load(paste0('~/dissdata/R/',agency,'_A.RData'))
	# limit the plot range per agency
	Amin = switch( agency, 'Muni'=.3, 'MBTA'=0, 'TTC'=0, 'JTA'=0 ) 
	Amax = switch( agency, 'Muni'=.9, 'MBTA'=.25, 'TTC'=.4, 'JTA'=.2 ) 
	# do the KDE
	x = A[['sched','cum','45','A_oh']]
	y = l2(A[['retro','cum','45','A_oh']] / A[['sched','cum','45','A_oh']])
	kde_result = kde( 
		cbind(x=c(x),y=c(y)),
		xmin=c(Amin-.1,-1),xmax=c(Amax+.1,1),
		gridsize=c(300,300)
	)
	pdf(paste0(figures_dir,agency,'-A_oh-cum.pdf'),width=5.5,height=5.5)
	par( mai=c(0.8,0.75,0.2,.1), pch='+', family='serif' ) # bottom, left, top, right
	# slice, persp, image, filled.contour
	plot( 
		kde_result, display='filled.contour', cont = contours, #col=heat.colors(6),
		bty='n', xaxt="n", yaxt="n", xlab='', ylab='',
		xlim=c(Amin,Amax),ylim=c(-1,1)
	)
	# add the axes: A_oh scheduled and percent difference (error)
	axis( 1, at=seq(Amin,Amax,by=0.1), las=0, pos=-1.1 )
	ps = c(.5,.6,.70,.8,.9,1,1.15,1.3,1.5,1.75,2)
	axis( 2, at=l2(ps), labels=sprintf('%+.0f%%',(ps-1)*100), las=2, pos=Amin+(Amin-Amax)/20 )
	#title(xlab="Scheduled Travel Time", line=2.5)
	# grid lines
	abline(h=l2(ps),v=seq(Amin,Amax,by=0.1),col=rgb(0,0,0,alpha=0.1)) # grey grid
	# add contours
	plot( kde_result, display='slice', cont=contours, add=T )
	# add agency name in top right corner
	text(x=Amax-.05,y=0.95,labels=agency,pos=1,cex=2)
	dev.off()
}

# clean up
remove(kde_result,contours,Amax,Amin,ps)
gc()

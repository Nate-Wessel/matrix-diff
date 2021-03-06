figures_dir = '/home/nate/Dropbox/diss/paper/figures/'

# selected origins identified by letters
origins = list(
	list( name='A', agency = 'TTC', o = '240', func = 'cum', param='45'), # same service, temporal misalignment
	list( name='B', agency = 'TTC', o = '287', func = 'cum', param='45'), # high access
	list( name='C', agency = 'JTA', o = '129', func = 'cum', param='45'), # positive error
	list( name='D', agency = 'JTA', o = '230', func = 'cum', param='45'), # high access
	list( name='E', agency = 'MBTA', o = '206', func = 'cum', param='45'), # mid range negative error
	list( name='F', agency = 'MBTA', o = '6', func = 'cum', param='45'), # high access
	list( name='G', agency = 'Muni', o = '41', func = 'cum', param='30'), # 
	list( name='H', agency = 'Muni', o = '190', func = 'cum', param='30') # 
)

# make a plot for each selected origin locations
for(O in origins){
	print(O$name)
	# get data
	load(paste0('~/dissdata/R/',O$agency,'_A.RData'))
	# subset A_ot to a one rush hour for schedule and retro
	Ar_ot = A[['retro',O$func,O$param,'A_ot']]
	As_ot = A[['sched',O$func,O$param,'A_ot']]
	ti = dimnames(Ar_ot)[[2]] >= "2017-11-10 16:00:00" & dimnames(Ar_ot)[[2]] <= "2017-11-10 19:00:00"
	Ar_ot = Ar_ot[O$o,ti]
	As_ot = As_ot[O$o,ti]
#	if(O$name=='D'){4*'u'}
	# range in accessibility rounded to percentages
	A_range = range(c(As_ot,Ar_ot))
	A_range[1] = floor(A_range[1]*100)/100
	A_range[2] = ceiling(A_range[2]*100)/100
	# plot the change over time
	pdf(paste0(figures_dir,'o_',O$name,'.pdf'),width=6,height=1.5)
		par(mar=c(1.5,0,.1,2.5),family='serif',bty='n') # bottom, left, top, right,
		plot( x=1:length(As_ot), y=As_ot, 
			type='l',col='blue', xaxt="n",yaxt='n',
			ylim=A_range, xlim=c(-3,180)
		)
		text(x=-5,y=A_range[2],labels=O$name,cex=2,pos=1)
		lines( x=1:length(Ar_ot), y=Ar_ot, type='l',col='red')
		# hours on the X axis
		axis( 1, at=c(0,60,120,180), labels=c('4pm','5pm','6pm','7pm'), las=0, tick=F, line=-1)
		# access on the y axis
		labs = c(A_range,round(mean(A_range),2))
		axis( 4, at=labs, labels=paste0(labs*100,'%'),las=1,tick=F,hadj=.5)
		# grid lines
		abline(v=c(0,60,120,180),h=labs,col=rgb(0,0,0,alpha=.3),lty=2)
	dev.off()
}
#remove('T',ti,Ar_ot,As_ot)
figures_dir = '/home/nate/Dropbox/diss/paper/figures/'

# selected origins identified by letters
origins = list(
	list(
		name='A', agency = 'TTC', o = '78', 
		func = 'negexp',param='30',
		start = "2017-11-10 16:00:00",
		end = "2017-11-10 19:00:00"
	),
	list(
		name='B', agency = 'TTC', o = '79', 
		func = 'negexp',param='30',
		start = "2017-11-10 16:00:00",
		end = "2017-11-10 19:00:00"
	)
)

# make a plot for each selected origin locations
for(O in origins){
	print(O$name)
	# get data
	load(paste0('~/dissdata/R/',O$agency,'_A.RData'))
	# subset A_ot to a one rush hour for schedule and retro
	Ar_ot = A[['retro',O$func,O$param,'A_ot']]
	As_ot = A[['sched',O$func,O$param,'A_ot']]
	ti = dimnames(Ar_ot)[[2]] >= O$start & dimnames(Ar_ot)[[2]] <= O$end
	Ar_ot = Ar_ot[O$o,ti]
	As_ot = As_ot[O$o,ti]
	# plot the change over time
	pdf(paste0(figures_dir,'o_',O$name,'.pdf'),width=6,height=2)
		par(mar=c(2,0,.1,2.5),family='serif',bty='n') # bottom, left, top, right,
		plot( x=1:length(Ar_ot), y=Ar_ot, 
			type='l',col='red', xaxt="n",xlab='',yaxt='n',
			ylim=range(c(As_ot,Ar_ot)),xlim=c(0,180)
		)
		lines( x=1:length(As_ot), y=As_ot, type='l',col='blue')
		# hours on the X axis
		axis( 1, at=c(0,60,120,180), labels=c('4pm','5pm','6pm','7pm'), las=0, tick=F, line=-1)
		abline(v=c(0,60,120,180),h=seq(0,1,by=.01),col=rgb(0,0,0,alpha=.3),lty=2)
		# access on the y axis
		axis( 4, at=seq(0,1,by=.01), labels=paste0(seq(0,100),'%'),las=1,tick=F,hadj=.5)
	dev.off()
}
#remove('T',ti,Ar_ot,As_ot)
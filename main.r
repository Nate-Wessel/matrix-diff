source('~/matrix-diff/read-data.r')
source('~/matrix-diff/access-functions.r')
source('~/matrix-diff/all-access.r')

figures_dir = '/home/nate/Dropbox/diss/paper/figures/'

# calculate various correlations between the two matrices
#cor_NA <- cor( is.na(s_odt), is.na(r_odt) )
cor_time_no_NA <- cor( x=s_odt, y=r_odt, use='pairwise.complete.obs', method='pearson' )


# plot the travel times (sampled)
pdf(paste0(figures_dir,agency,'-time.pdf'),width=5.5,height=5.5)
	par( mai=c(0.7,0.7,0.4,0.42), pch='+', family='serif' )
	# sample 15k non-null value pairs
	s = sample(length(s_odt),10^5)
	s = s[ !is.na(s_odt[s]) & !is.na(r_odt[s]) ]
	s = s[1:15000]
	# limit the plot range to the 98th percentile
	plot_limit = quantile(c(s_odt[s],r_odt[s]),0.99)/3600 
	plot(
		x=s_odt[s]/3600, xlab='',
		y=r_odt[s]/3600, ylab='',
		main=paste(agency,'Travel times'),
		bty='n', asp=1, xaxt="n", yaxt="n",
		col=rgb(0,0,0,alpha=0.1),
		xlim=c(0,plot_limit),ylim=c(0,plot_limit)
	)
	abline(0,1,col='red')
	axis( 2, at=c(0:5), labels=paste(0:5,'h'), las=2, pos=-plot_limit/30 )
	axis( 1, at=c(0:5), labels=paste(0:5,'h'), las=0, pos=-plot_limit/30 )
	abline(h=0:5,v=0:5,col=rgb(0,0,0,alpha=0.05))
	title(ylab="Retro", line=2)
	title(xlab="Schedule", line=2)
dev.off()

# plot A_ot schedule vs retro (sampled)
#cor(c(A[['sched','gauss','A_ot']]),c(A[['retro','gauss','A_ot']]))
pdf(paste0(figures_dir,agency,'-A_ot.pdf'),width=5.5,height=5.5)
	par(mai=c(0.7,0.7,0.4,0.42), family='serif') # bottom left top right
	samp_i = sample(length(A[['sched','gauss','A_ot']]),10^4)
	plot_limit = quantile( # limit plot to 99th percentile
		c(A[['sched','gauss','A_ot']][samp_i],A[['retro','gauss','A_ot']][samp_i]
	),0.999)
	plot(
		x=A[['sched','gauss','A_ot']][samp_i], xlab='',
		y=A[['retro','gauss','A_ot']][samp_i], ylab='',
		main=paste(agency,'A_ot'), pch='+', bty='n', 
		asp=1, xaxt="n", yaxt="n", col=rgb(0,0,0,alpha=0.1),
		xlim=c(0,plot_limit),ylim=c(0,plot_limit)
	)
	abline(0,1,col='red')
	labs = seq(0,1,by=.1)
	axis( 2, at=labs, labels=labs, las=2, pos=-plot_limit/30 )
	axis( 1, at=labs, labels=labs, las=0, pos=-plot_limit/30 )
	abline(h=labs,v=labs,col=rgb(0,0,0,alpha=0.05))
	title(ylab="Retro", line=2)
	title(xlab="Schedule", line=2)
dev.off()

# plot A_o schedule vs retro
pdf(paste0(figures_dir,agency,'-A_o.pdf'),width=5.5,height=5.5)
	par( mai=c(0.7,0.7,0.4,0.42), pch='+', family='serif' )
	# plot all points
	plot_limit = max(c(A[['sched','gauss','A_o']],A[['retro','gauss','A_o']]))
	plot(
		x=A[['sched','gauss','A_o']], xlab='',
		y=A[['retro','gauss','A_o']], ylab='',
		main=paste(agency,'A_o'),
		pch='+', bty='n', asp=1, xaxt="n", yaxt="n",
		col=rgb(0,0,0,alpha=0.4)
	)
	Ao_lm = lm( A[['retro','gauss','A_o']] ~ A[['sched','gauss','A_o']] )
	abline(Ao_lm,col='blue') # regression line
	abline(0,1,col='red') # identity line
	labs = seq(0,1,by=.1)
	axis( 2, at=labs, labels=labs, las=2, pos=-plot_limit/30 )
	axis( 1, at=labs, labels=labs, las=0, pos=-plot_limit/30 )
	abline(h=labs,v=labs,col=rgb(0,0,0,alpha=0.05))
	title(ylab="Retro", line=2)
	title(xlab="Schedule", line=2)
dev.off()


# plot the change in selected A_ot over t
# first select the O's to use
if(agency=='JTA'){ 
	os = c( '82' )
	ylim = c(0,.10)
	period_start = "2017-11-10 16:00:00"
	period_end = "2017-11-10 19:00:00"	
}else if(agency=='TTC'){ 
	os = c( 19, 236, 116, 240 ) 
	ylim = c(0,.15)
	period_start = "2017-11-10 16:00:00"
	period_end = "2017-11-10 19:00:00"
}
Ar_ot = A[['retro','gauss','A_ot']]
As_ot = A[['sched','gauss','A_ot']]

# subset A_ot to a one (rush hour) period
sub_i = dimnames(Ar_ot)[[2]] >= period_start & dimnames(Ar_ot)[[2]] <= period_end
sub_Ar_ot = Ar_ot[,sub_i]
sub_As_ot = As_ot[,sub_i]
# sort by correlation between schedule and retro
cm = abind( fri_Ar_ot, fri_As_ot, along=3, new.names=c('r','s') )
so = sort( apply( X=cm, c(1), function(m){ cor(m[,'r'],m[,'s']); } ) )
# plot
pdf(paste0(figures_dir,agency,'-A_ot-over-t.pdf'),width=8,height=4)
	par(mar=c(4.5,3,3,1),family='serif',bty='n')
	plot(
		0, type='n', xlim=c(0,181), ylim=ylim,
		xlab='', ylab='A_ot', main=paste(agency,'A_ot over t'),
		xaxt="n"
	)
	for(o in os){
		lines(x=1:181,y=fri_Ar_ot[o,],col='red')
		lines(x=1:181,y=fri_As_ot[o,],col='black')
	}
	axis( 1, at=c(0,60,120,180), labels=c('4pm','5pm','6pm','7pm'), las=0, pos=-.02 ) # X
dev.off()



# plot the access functions
#pdf(paste0(figures_dir,'A-funcs.pdf'),width=5,height=3)
#	par(mar=c(4,2,3,1),family='serif')
#	plot( 
#		0, type='n', ylim=c(0,1), xlim=c(0,120), bty='n', xaxt="n", yaxt="n",
#		main='Accessibility Functions',xlab=NA, ylab=NA
#	)
#	x = 0:(120*60)
#	lines(x=x/60,y=gauss(x),col='red')
#	lines(x=x/60,y=cum(x),col='darkgreen')
#	lines(x=x/60,y=negexp(x),col='blue')
#	axis( 2, at=c(0,1), labels=c(0,1), las=2, pos=-5 ) # left
#	axis( 1, at=c(0,30,60,90,120), labels=c('0','30 minutes','1 hour','1.5 hours','2 hours'), las=0, pos=-.1 ) # bottom
#	text(x=c(32,60,15),y=c(.8,.6,.3),labels=c('Gaussian','Cumulative','Negative\nExponential'),col=c('red','darkgreen','blue'))
#dev.off()



# subtract the temporal mean from the schedule times
s_odt_noise = trunc( s_odt - c( apply( s_odt, c(1,2), mean, na.rm=T ) ) )
r_odt_noise = trunc( r_odt - c( apply( r_odt, c(1,2), mean, na.rm=T ) ) )
cor(s_odt_noise,r_odt_noise,use='pairwise.complete.obs', method='pearson')

samp_i = sample(length(s_odt_noise),10^4.3)
plot(
	x=s_odt_noise[samp_i]/60, xlab='Schedule',
	y=r_odt_noise[samp_i]/60, ylab='Retro',
	main='Travel time comparison, NOISE',
	pch='+', bty='n',	family='serif',
	col=rgb(0,0,0,alpha=0.1),
	xlim=c(-30,30),ylim=c(-30,30)
)
remove(samp_i)

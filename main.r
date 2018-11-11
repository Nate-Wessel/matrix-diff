#source('~/matrix-diff/read-data.r')
#source('~/matrix-diff/access-functions.r')
#source('~/matrix-diff/all-access.r')

agency = 'JTA'
load(paste0('~/',agency,'.RData'))
# loads: 
#	agency	: agency name
#	od			: OD polygon table
#	wt			: walking times
#	s_odt		: schedule
#	r_odt		: retro
#	A			: access scores

library('tidyverse')

figures_dir = '/home/nate/Dropbox/diss/paper/figures/'


# plot A_ot schedule vs retro (sampled)
pdf(paste0(figures_dir,agency,'-A_ot.pdf'),width=5.5,height=5.5)
	par(mai=c(0.7,0.7,0.4,0.42), family='serif') # bottom left top right
	samp_i = sample(length(A[['sched','gauss','A_ot']]),10^4)
	plot_limit = quantile( # limit plot to 99.9th percentile
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
	axis( 2, at=labs, labels=labs, las=2, pos=-xlim[2]/30 )
	axis( 1, at=labs, labels=labs, las=0, pos=-xlim[2]/30 )
	abline(h=labs,v=labs,col=rgb(0,0,0,alpha=0.05))
	title(ylab="Retro", line=2)
	title(xlab="Schedule", line=2)
dev.off()


# plot A_o schedule vs retro
pdf(paste0(figures_dir,agency,'-A_o2.pdf'),width=5.5,height=5.5) 
	par( mai=c(0.7,1,0.4,0.42), pch='+', family='serif' ) # bottom, left, top, right
	xlim = switch(agency, 'Muni'=c(.1,.4), 'MBTA'=c(0,.2), 'TTC'=c(0,.2), 'JTA'=c(0,.2) ) 
	# plot all points
	plot(
		x=A[['sched','negexp','A_o']], # scheduled score
		y=log( A[['retro','negexp','A_o']] / A[['sched','negexp','A_o']] ), # logged pct diff
		main=paste(agency,'A_o'), pch='+', bty='n', yaxt="n", xaxt='n', xlab='', ylab='',
		xlim=xlim, ylim=log(c(.7,1.3)),
		col=rgb(0,0,0,alpha=0.4)
	)
	lab = seq(xlim[1],xlim[2],by=.1)
	axis(1,at=lab,labels=lab,las=2,pos=log(0.69),las=0)
	ps = c(.70,.8,.9,1,1.15,1.3)
	axis( 2, at=log(ps), labels=sprintf('%+.1f%%',(ps-1)*100), las=2, pos=xlim[1]-diff(xlim)/30 )
	abline(h=log(ps),v=lab,col=rgb(0,0,0,alpha=0.05))
	abline(h=0,col=rgb(1,0,0,alpha=0.3))
	title(ylab="Retro Access % Difference", line=4)
	title(xlab="Schedule Access at Origin", line=2)
dev.off()


# plot the change in selected A_ot over t
# first select the O's to use
if(agency=='JTA'){ 
	os = c( '82' )
	ylim = c(0,.10)
	period_start = "2017-11-10 16:00:00"
	period_end = "2017-11-10 19:00:00"	
}else if(agency=='TTC'){ 
	os = c( 19, 235, 115, 239 ) 
	ylim = c(0,.3)
	period_start = "2017-11-10 16:00:00"
	period_end = "2017-11-10 19:00:00"
}else if(agency=='MBTA'){ 
	os = c( 166, 98 ) 
	ylim = c(0,.3)
	period_start = "2017-11-06 06:00:00"
	period_end = "2017-11-06 10:00:00"
}
Ar_ot = A[['retro','gauss','A_ot']]
As_ot = A[['sched','gauss','A_ot']]

# subset A_ot to a one (rush hour) period
sub_i = dimnames(Ar_ot)[[2]] >= period_start & dimnames(Ar_ot)[[2]] <= period_end
sub_Ar_ot = Ar_ot[,sub_i]
sub_As_ot = As_ot[,sub_i]
# sort by correlation between schedule and retro
cm = abind( sub_Ar_ot, sub_As_ot, along=3, new.names=c('r','s') )
#so = sort( apply( X=cm, c(1), function(m){ cor(m[,'r'],m[,'s']); } ) )
# plot
pdf(paste0(figures_dir,agency,'-A_ot-over-t.pdf'),width=8,height=4)
	par(mar=c(4.5,3,3,1),family='serif',bty='n')
	plot(
		0, type='n', xlim=c(0,sum(sub_i)), ylim=ylim,
		xlab='', ylab='A_ot', main=paste(agency,'A_ot over t'),
		xaxt="n"
	)
	for(o in os){
		lines(x=1:sum(sub_i),y=sub_Ar_ot[o,],col='red')
		lines(x=1:sum(sub_i),y=sub_As_ot[o,],col='black')
	}
	axis( 1, at=c(0,60,120,180), labels=c('4pm','5pm','6pm','7pm'), las=0, pos=-.02 ) # X
dev.off()


#############################
# Plot moving averages of A_t
#############################
library(forecast) # gets moving averages ( ma() )
# subset A_ot to just one rush hour period
Ar_ot = A[['retro','gauss','A_ot']]
As_ot = A[['sched','gauss','A_ot']]
sub_i = dimnames(As_ot)[[2]] >= '06:00:00' & dimnames(As_ot)[[2]] <= '10:00:00'
sub_Ar_ot = Ar_ot[,sub_i]
sub_As_ot = As_ot[,sub_i]
sta = ma( apply(sub_As_ot,2,mean), 20 )
rta = ma( apply(sub_Ar_ot,2,mean), 20 )

plot(x=1:length(rta),y=rta/sta,type='l')


# subtract the temporal mean from the schedule times
#s_odt_noise = trunc( s_odt - c( apply( s_odt, c(1,2), mean, na.rm=T ) ) )
#r_odt_noise = trunc( r_odt - c( apply( r_odt, c(1,2), mean, na.rm=T ) ) )
#cor(s_odt_noise,r_odt_noise,use='pairwise.complete.obs', method='pearson')

#samp_i = sample(length(s_odt_noise),10^4.3)
#plot(
#	x=s_odt_noise[samp_i]/60, xlab='Schedule',
#	y=r_odt_noise[samp_i]/60, ylab='Retro',
#	main='Travel time comparison, NOISE',
#	pch='+', bty='n',	family='serif',
#	col=rgb(0,0,0,alpha=0.1),
#	xlim=c(-30,30),ylim=c(-30,30)
#)
#remove(samp_i)

source('~/matrix-diff/read-data.r')
source('~/matrix-diff/access-functions.r')

figures_dir = '/home/nate/Dropbox/diss/paper/figures/'

# calculate various correlations between the two matrices
#cor_NA <- cor( is.na(s_odt), is.na(r_odt) )
#cor_time_no_NA <- cor( x=s_odt, y=r_odt, use='pairwise.complete.obs', method='pearson' )

# plot the correlation of the travel times (sampled)
pdf(paste0(figures_dir,agency,'-time-corr.pdf'),width=5.5,height=5.5)
	par(mar=c(4.5,3,3,1))
	samp_i = sample(length(s_odt),10^4.27)
	plot_samp_n = sum(!is.na(s_odt[samp_i] + r_odt[samp_i]))
	plot(
		x=s_odt[samp_i]/3600, xlab='Schedule',
		y=r_odt[samp_i]/3600, ylab='',
		main=paste(agency,'Travel times'),
		pch='+', bty='n',family='serif', asp=1, xaxt="n", yaxt="n",
		col=rgb(0,0,0,alpha=0.1),
		xlim=c(0,5),ylim=c(0,5)
	)
	abline(0,1,col='red')
	axis( 2, at=c(0:5), labels=paste(0:5,'h'), las=2, pos=-.2 )
	axis( 1, at=c(0:5), labels=paste(0:5,'h'), las=0, pos=-.2 )
	abline(h=0:5,v=0:5,col=rgb(0,0,0,alpha=0.05))
	title(ylab="Retro", line=1.3)
dev.off()

# plot sample of A_ot schedule vs retro
s_acc = access( s_odt, od$zones, od$zones, gauss30 )
r_acc = access( r_odt, od$zones, od$zones, gauss30 )
As_ot = s_acc[[3]]
Ar_ot = r_acc[[3]]
pdf(paste0(figures_dir,agency,'-A_ot-corr.pdf'),width=5.5,height=5.5)
	par(mar=c(4.5,3,3,1))
	samp_i = sample(length(As_ot),10^4)
	plot(
		x=As_ot[samp_i], xlab='Schedule',
		y=Ar_ot[samp_i], ylab='',
		main=paste(agency,'A_ot'),
		pch='+', bty='n',family='serif', asp=1, xaxt="n", yaxt="n",
		col=rgb(0,0,0,alpha=0.1),
		xlim=c(0,.3),ylim=c(0,.3)
	)
	abline(0,1,col='red')
	labs = seq(0,.3,by=.05)
	axis( 2, at=labs, labels=labs, las=2, pos=-.02 )
	axis( 1, at=labs, labels=labs, las=0, pos=-.02 )
	abline(h=labs,v=labs,col=rgb(0,0,0,alpha=0.05))
	title(ylab="Retro", line=2)
dev.off()

# plot A_o schedule vs retro
As_o = s_acc[[2]]
Ar_o = r_acc[[2]]
pdf(paste0(figures_dir,agency,'-A_o-corr.pdf'),width=5.5,height=5.5)
	plot(
		x=As_o, xlab='Schedule',
		y=Ar_o, ylab='',
		main=paste(agency,'A_o, Schedule vs. Retro'),
		pch='+', bty='n',family='serif', asp=1, xaxt="n", yaxt="n",
		col=rgb(0,0,0,alpha=0.4)
	)
	abline(0,1,col='red')
	labs = seq(0,.3,by=.05)
	axis( 2, at=labs, labels=labs, las=2, pos=-.01 )
	axis( 1, at=labs, labels=labs, las=0, pos=-.01 )
	abline(h=labs,v=labs,col=rgb(0,0,0,alpha=0.05))
	title(ylab="Retro", line=2.5)
dev.off()

# order of zone accessibility
#so = sort(s_acc[[2]])
#ro = sort(s_acc[[2]])

# plot the change in selected A_ot over t
# first select the o
if(agency=='JTA'){ 
	os = c( 126 ) 
	ylim = c(0,.15)
}else if(agency=='TTC'){ 
	os = c( 19, 236, 116, 240 ) 
	ylim = c(0,.5)
}
# subset A_ot to a one rush hour period
sub_i = dimnames(Ar_ot)[[2]] >= "2017-11-10 16:00:00" & dimnames(Ar_ot)[[2]] <= "2017-11-10 19:00:00"
fri_Ar_ot = Ar_ot[,sub_i]
fri_As_ot = As_ot[,sub_i]
pdf(paste0(figures_dir,agency,'-A_ot-over-t.pdf'),width=8,height=6)
	plot(
		0, type='n', xlim=c(0,181), ylim=ylim,
		xlab='time', ylab='A_o', main=paste(agency,'A_o over t'),
		family='serif',bty='n'
	)
	for(o in os){
		lines(x=1:181,y=fri_Ar_ot[o,],col='red')
		lines(x=1:181,y=fri_As_ot[o,],col='black')
	}
dev.off()






# PLAYSPACE
t_range = 794:974
o='235'
d='X254'
plot(
	y=r_odt[o,d,t_range]/60, x=t_range,
	type='l',col='red',
	ylim=c(0,120)
)
lines(
	y=s_odt[o,d,t_range]/60, x=t_range,
	col='blue'
)
polygon(
	y=c(r_odt[o,d,t_range]/60,rev(s_odt[o,d,t_range]/60)), 
	x=c(t_range,rev(t_range)),
	fillOddEven=T,col=rgb(0,0,0,alpha=0.1),border=NA
)





# PLAYSPACE
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

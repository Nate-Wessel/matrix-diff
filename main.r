source('read-data.r')
source('access-functions.r')

# clip travel times to walking times
i = !is.na(c(wt)) & c(s_odt) > c(wt) | is.na(s_odt)
s_odt[ i ] = c(wt)[i]
i = !is.na(c(wt)) & c(r_odt) > c(wt) | is.na(r_odt)
r_odt[ i ] = c(wt)[i]
remove(i)
# set trips over 5 hours to NA
s_odt[s_odt>5*3600] = NA
r_odt[r_odt>5*3600] = NA


# calculate various correlations between the two matrices
#cor_NA <- cor( is.na(s_odt), is.na(r_odt) )
#cor_time_no_NA <- cor( x=s_odt, y=r_odt, use='pairwise.complete.obs', method='pearson' )


figures_dir = '/home/nate/Dropbox/diss/paper/figures/'


# plot the correlation of the travel times (sampled)
pdf(paste0(figures_dir,agency,'-time-corr.pdf'),width=8,height=6)
	samp_i = sample(length(s_odt),10^4.27)
	plot_samp_n = sum(!is.na(s_odt[samp_i] + r_odt[samp_i]))
	plot(
		x=s_odt[samp_i]/3600, xlab='Schedule',
		y=r_odt[samp_i]/3600, ylab='Retro',
		main=paste(agency,'Travel times, Schedule vs. Retro'),
		pch='+', bty='n',family='serif',
		col=rgb(0,0,0,alpha=0.1),
		xlim=c(0,5),ylim=c(0,5)
	)
	abline(0,1,col='red')
dev.off()

# plot sample of A_ot schedule vs retro
s_acc = access( s_odt, od$zones, od$zones, gauss30 )
r_acc = access( r_odt, od$zones, od$zones, gauss30 )
As_ot = s_acc[[3]]
Ar_ot = r_acc[[3]]
pdf(paste0(figures_dir,agency,'-A_ot-corr.pdf'),width=8,height=6)
	samp_i = sample(length(As_ot),10^4)
	plot(
		x=As_ot[samp_i], xlab='A_ot Schedule',
		y=Ar_ot[samp_i], ylab='A_ot Retro',
		main=paste(agency,'A_ot, Schedule vs. Retro'),
		pch='+', bty='n',family='serif',
		col=rgb(0,0,0,alpha=0.1),
		xlim=c(0,.5),ylim=c(0,.5)
	)
	abline(0,1,col='red')
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

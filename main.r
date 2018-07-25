source('read-data.r')

# ------------- clip to walking times -----------------------
# or alternately set max of times to the walking time
i = !is.na(c(wt)) & c(s_odt) > c(wt) | is.na(s_odt)
s_odt[ i ] = c(wt)[i]
i = !is.na(c(wt)) & c(r_odt) > c(wt) | is.na(r_odt)
r_odt[ i ] = c(wt)[i]
remove(i)
# ----------- set trips over x hours to NA ------------------
s_odt[s_odt>5*3600] = NA
r_odt[r_odt>5*3600] = NA

# calculate various correlations between the two matrices
cor_NA <- cor( is.na(s_odt), is.na(r_odt) )
cor_time_no_NA <- cor( x=s_odt, y=r_odt, use='pairwise.complete.obs', method='pearson' )

# plot the correlation of the travel times (sampled)
samp_i = sample(length(s_odt),10^4.27)
plot_samp_n = sum(!is.na(s_odt[samp_i] + r_odt[samp_i]))
plot(
	x=s_odt[samp_i]/3600, xlab='Schedule',
	y=r_odt[samp_i]/3600, ylab='Retro',
	main='Travel time comparison, Schedule vs. Retro',
	pch='+', bty='n',family='serif',
	col=rgb(0,0,0,alpha=0.1),
	xlim=c(0,5),ylim=c(0,5)
)
abline(0,1,col='red')

remove(samp_i)





# plot A_ot schedule vs retro
s_acc = access( s_odt, od@data[,'zones'], od@data[,'zones'], gauss )[[3]]
r_acc = access( r_odt, od@data[,'zones'], od@data[,'zones'], gauss )[[3]]

samp_i = sample(length(s_acc),10^4)
plot(
	x=s_acc[samp_i], xlab='Schedule',
	y=r_acc[samp_i], ylab='Retro',
	main='JTA A_ot, Schedule vs. Retro',
	pch='+', bty='n',family='serif',
	col=rgb(0,0,0,alpha=0.1),
	xlim=c(0,.5),ylim=c(0,.5)
)
abline(0,1,col='red')

remove(samp_i)






# plot the change in A_o over time
s_acc = access( s_odt, od@data[,'zones'], od@data[,'zones'], gauss )[[3]]
r_acc = access( r_odt, od@data[,'zones'], od@data[,'zones'], gauss )[[3]]

plot(
	0, type='n', xlim=c(0,181), ylim=c(0,.5),
	xlab='time', ylab='A_o', main='TTC A_o over t',
	family='serif',bty='n'
)
for(o in 1:181){
	lines(x=1:181,y=r_acc[o,],col=rgb(0,0,0,alpha=.2))
}






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

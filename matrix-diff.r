od_table        = 'jv_od'
schedule_dir    = '/home/nate/dissdata/jv-all_stops'
retro_dir       = '/home/nate/dissdata/jv-retro'
walk_times_file = '/home/nate/Dropbox/diss/analysis/walk-times/jta.csv'

od_table        = 'ttc_od'
schedule_dir    = '/home/nate/dissdata/ttc-sched'
retro_dir       = '/home/nate/dissdata/ttc-retro'
walk_times_file = '/home/nate/Dropbox/diss/analysis/walk-times/ttc.csv'

library('abind')
library('RPostgreSQL')
library('postGIStools')

# database connection
con <- dbConnect( PostgreSQL(), dbname='diss', user='nate', host='localhost', password='mink')
od <- get_postgis_query(con,paste('SELECT * FROM',od_table,'ORDER BY uid'),geom_name='voronoi_geom')
# add zone counts (one per, obviously) for simple weighting later
od$zones = 1
# plot the variables so far
#spplot(od,c('workforce','jobs'))

# read one of the matrix files output by the OTP script
read_time_matrix <- function(filepath){
	print(filepath)
	t = read.csv( filepath, stringsAsFactors=F, na.strings="-", row.names=1 )
	m = as.matrix( t )
	# remove 'X's from column names
	colnames(m) <- substr( colnames(m),2,10)
	return( m )
}

# get the walking time matrix
wt = read_time_matrix(walk_times_file)

# get all files from the retro data into a timecube
# takes the folder/network name
read_timecube <- function(directory){
	file_paths = list.files( directory, full.names=T )
	# the new dimnames are derived from the file names (- .csv)
	dates = sub( '\\..*$', '', basename(file_paths) )
	list_of_matrices <- lapply( file_paths, read_time_matrix )
	# this is a 3D array OxDxT
	m = abind( list_of_matrices, along = 3, new.names = dates )
	return( m )
}

access <- function( times_odt, weights_o, weights_d, acc_func ){
	# apply an accessibility function over d, returning sum for o,t
	# assert that things are the right size
	stopifnot( dim(times_odt)[1] == length(weights_d) )
	stopifnot( dim(times_odt)[1] == dim(times_odt)[2] )
	stopifnot( length(weights_o) == length(weights_d) )
	# need to permute array in order to use vector recycling on weights
	times_dot = aperm( times_odt, c(2,1,3) )
	# d,o,t -> o,t by summing over destinations
	acc_ot = apply( acc_func(times_dot) * weights_d, c(2,3), sum, na.rm=T ) / sum(weights_d)
	# o,t -> o by averaging over times (all times given same weight)
	acc_o = apply( acc_ot, c(1), mean, na.rm=T )
	# total summary measure with o weights (o weights get recycled)
	acc = sum( acc_ot * weights_o ) / sum( weights_o * dim(acc_ot)[2] ) 
	return(list( acc, acc_o, acc_ot ))
}

# read in the travel time data
s_odt <- read_timecube(schedule_dir)
r_odt <- read_timecube(retro_dir)
# remove two ODs (jta only)
#s_odt <- s_odt[-c(19,210),-c(19,210),]
#r_odt <- r_odt[-c(19,210),-c(19,210),]
#wt <- wt[-c(19,210),-c(19,210)]

# ---------------SUBSET TO COMMON moments-------------------
# rename S by time rather than date since it only has one day
dimnames(s_odt)[[3]] <- substr( dimnames(s_odt)[[3]],12,19)
# select times that both have in common
common_times = intersect( 
	substr( dimnames(r_odt)[[3]],12,19), 
	dimnames(s_odt)[[3]] )
# subset both to only common times, regardless of date
s_odt <- s_odt[,,common_times]
r_subset = grep( 
	pattern=paste(paste0('*',common_times),collapse='|'),
	x=dimnames(r_odt)[[3]] )
r_odt <- r_odt[,, r_subset ]
# expand/repeat the schedule out to the dimensions of the retro-data
r_times = substr( dimnames(r_odt)[[3]],12,19)
s_odt = s_odt[,,r_times]
# clean up a bit
remove(common_times,r_subset,r_times)
# -----------------------------------------------------------

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


# --------- calculate access for each weighting scheme -------------- 
# get access to jobs by o,t for both schedule and retro
# first define accessibility funcs
bin45 <- function(t){ return( t/60L <= 45L ) }
bin90 <- function(t){ return( t/60L <= 90L ) }
gauss <- function(t){ 
	bandwidth = 45 * 60
	return( exp(-(t**2 / (2*bandwidth**2))) ) 
}
acc_results = array(NA,c(3,3,2))
dimnames(acc_results)[[1]] = c('gauss','bin45','bin90') # cost function names
dimnames(acc_results)[[2]] = c('cor_ot','cor_o','retro/sched') # correlation measures
dimnames(acc_results)[[3]] = c('workforce/jobs','zones/zones') # weighting schemes

for( weighting_scheme in dimnames(acc_results)[[3]] ){
	print(weighting_scheme)
	w_names = strsplit(weighting_scheme,'/')[[1]]
	w1 = w_names[1]
	w2 = w_names[2]
	# for each of these access functions
	for( func_name in dimnames(acc_results)[[1]] ){
		print( func_name )
		func = get(func_name)
		# do the accessibility calculations
		s_acc = access( s_odt, od@data[,w1], od@data[,w2], func )
		r_acc = access( r_odt, od@data[,w1], od@data[,w2], func )
		# pull out the measures and put them in the result matrix
		acc_results[func_name,'cor_ot',weighting_scheme] =  cor( c(s_acc[[3]]), c(r_acc[[3]]) )
		acc_results[func_name,'cor_o',weighting_scheme] = cor( s_acc[[2]], r_acc[[2]] )
		acc_results[func_name,'retro/sched',weighting_scheme] = r_acc[[1]] / s_acc[[1]] 
	}
}
remove(func,func_name,weighting_scheme,w1,w2,w_names)
# ------------------------------------------------------------------







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

# Produces:
# agency - agency acronym passed along to be sure we know what we're working with
# od     - a spatialpolygons dataframe of the OD zones
# wt     - a walking time matrix
# s_odt  - scheduled travel times indexed by o,d,t
# r_odt  - retro travel times indexed by o,d,t
agency = 'TTC'
if( agency == 'JTA' ){
	od_table        = 'jv_od'
	schedule_dir    = '~/dissdata/jv-all-stops'
	retro_dir       = '~/dissdata/jv-retro'
	walk_times_file = '~/Dropbox/diss/analysis/walk-times/jta.csv'
}else if( agency == 'TTC' ){
	od_table        = 'ttc_od'
	schedule_dir    = '~/dissdata/ttc-sched'
	retro_dir       = '~/dissdata/ttc-retro'
	walk_times_file = '~/Dropbox/diss/analysis/walk-times/ttc.csv'
}else if( agency == 'MBTA' ){
	od_table        = 'mbta_od'
	schedule_dir    = '~/dissdata/mbta-sched'
	retro_dir       = '~/dissdata/mbta-retro'
	walk_times_file = '~/Dropbox/diss/analysis/walk-times/mbta.csv'
}else if( agency == 'Muni' ){
	od_table        = 'muni_od'
	schedule_dir    = '~/dissdata/muni-sched'
	retro_dir       = '~/dissdata/muni-retro'
	walk_times_file = '~/Dropbox/diss/analysis/walk-times/muni.csv'
}

# read one of the matrix files output by the OTP script
read_time_matrix <- function(filepath){
	print(filepath)
	t = read.csv( filepath, stringsAsFactors=F, na.strings="-", row.names=1 )
	m = as.matrix( t )
	# remove 'X's from column names
	colnames(m) <- substr( colnames(m),2,10)
	return( m )
}

library('abind')
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

# get the walking time matrix as double, convert to integer
wt = read_time_matrix(walk_times_file)
wt = array(as.integer(wt),dim(wt))
# read in the travel time data
s_odt <- read_timecube(schedule_dir)
r_odt <- read_timecube(retro_dir)

# remove any problem OD's
if( agency == 'JTA' ){
	bad_od = c(19,210,64,295,87,246,294,141,152,172,150,214,102,236,111)
	s_odt <- s_odt[-bad_od,-bad_od,]
	r_odt <- r_odt[-bad_od,-bad_od,]
	wt    <-    wt[-bad_od,-bad_od]
}else if( agency == 'Muni' ){
	bad_od = c(5,127,110,246,207,133)
	s_odt <- s_odt[-bad_od,-bad_od,]
	r_odt <- r_odt[-bad_od,-bad_od,]
	wt    <-    wt[-bad_od,-bad_od]
}else if( agency == 'TTC' ){
	bad_od = c(38,295,147,174,8,94)
	s_odt <- s_odt[-bad_od,-bad_od,]
	r_odt <- r_odt[-bad_od,-bad_od,]
	wt    <-    wt[-bad_od,-bad_od]
}else if( agency == 'MBTA' ){
	bad_od = c(73,67,165,250,87,57)
	s_odt <- s_odt[-bad_od,-bad_od,]
	r_odt <- r_odt[-bad_od,-bad_od,]
	wt    <-    wt[-bad_od,-bad_od]
}
remove(bad_od)

# clip travel times to walking times
i = !is.na(c(wt)) & c(s_odt) > c(wt) | is.na(s_odt)
s_odt[ i ] = c(wt)[i]
i = !is.na(c(wt)) & c(r_odt) > c(wt) | is.na(r_odt)
r_odt[ i ] = c(wt)[i]
remove(i)

# set any times == 0 to 1
s_odt[s_odt==0] = 1L
r_odt[r_odt==0] = 1L
	
# ------------- Subset matrices to shared time ----------------

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

# clean up variables that don't need to go past this script
remove(schedule_dir,retro_dir,walk_times_file,od_table,con)
remove(common_times,r_subset,r_times)
remove(read_timecube, read_time_matrix)
gc()

# now do the accessibility calculations before storing everything for later
source('~/matrix-diff/access-functions.r')
source('~/matrix-diff/all-access.r')

# save the data for this agency for quicker reading later
save(agency,s_odt,r_odt,file=paste0('~/dissdata/R/',agency,'_times','.RData'))
save(agency,A,file=paste0('~/dissdata/R/',agency,'_A.RData'))


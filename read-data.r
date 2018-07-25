# Produces:
# agency - agency acronym passed along to be sure we know what we're working with
# od     - a spatialpolygons dataframe of the OD zones
# wt     - a walking time matrix
# s_odt  - scheduled travel times indexed by o,d,t
# r_odt  - retro travel times indexed by o,d,t

agency = 'JTA'

if( agency == 'JTA' ){
	od_table        = 'jv_od'
	schedule_dir    = '/home/nate/dissdata/jv-all-stops'
	retro_dir       = '/home/nate/dissdata/jv-retro'
	walk_times_file = '/home/nate/Dropbox/diss/analysis/walk-times/jta.csv'
}else if( agency == 'TTC' ){
	agency           = 'TTC'
	od_table        = 'ttc_od'
	schedule_dir    = '/home/nate/dissdata/ttc-sched'
	retro_dir       = '/home/nate/dissdata/ttc-retro'
	walk_times_file = '/home/nate/Dropbox/diss/analysis/walk-times/ttc.csv'
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

# database connection
library('RPostgreSQL')
library('postGIStools')
con <- dbConnect( PostgreSQL(), dbname='diss', user='nate', host='localhost', password='mink')
od <- get_postgis_query(con,paste('SELECT * FROM',od_table,'ORDER BY uid'),geom_name='voronoi_geom')
# add zone counts (one per, obviously) for simple weighting later
od$zones = 1

# get the walking time matrix
wt = read_time_matrix(walk_times_file)
# read in the travel time data
s_odt <- read_timecube(schedule_dir)
r_odt <- read_timecube(retro_dir)

if( agency == 'JTA' ){
	# remove two ODs (jta only)
	s_odt <- s_odt[-c(19,210),-c(19,210),]
	r_odt <- r_odt[-c(19,210),-c(19,210),]
	wt    <-    wt[-c(19,210),-c(19,210)]
}
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




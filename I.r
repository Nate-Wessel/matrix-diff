# get access data
agency = 'Muni'
load(paste0('~/dissdata/R/',agency,'_A.RData'))
l2 = function(x){log(x,2)} # log base 2

# database connection
library('RPostgreSQL')
library('postGIStools')
library('spdep')
# grab OD points from the DB
od_table = switch(agency,'TTC'='ttc_od','JTA'='jv_od','Muni'='muni_od','MBTA'='mbta_od')
con <- dbConnect( PostgreSQL(), dbname='diss', user='nate', host='localhost', password='mink')
od <- get_postgis_query(con,paste('SELECT * FROM',od_table,'ORDER BY uid'),geom_name='loc_geom')
dbDisconnect(con)

# Sphere of Influence Graph as defined here: 
# https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
nb = tri2nb(od)
#plot.nb(nb,coordinates(od))
nb = graph2nb(soi.graph(nb, coordinates(od)))
#plot.nb(nb,coordinates(od))

# measure error hourly
eAo = A[['retro','cum','30','A_oh']] / A[['sched','cum','30','A_oh']]
#eAoh = exp(apply(log(A[['retro','cum','30','A_oh']] / A[['sched','cum','30','A_oh']]),2,mean))
eAh = A[['retro','cum','30','A_h']] / A[['sched','cum','30','A_h']]

mymoran = function(x){
	mi = moran(
		x=x,
		listw=nb2listw(nb,style='W'),
		n=length(od),
		S0=length(od)
	)
	return(mi$I)
}

ik = apply(l2(eAo),2,mymoran)
plot(l2(eAh),ik)
abline(lm(ik~l2(eAh)))

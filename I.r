# generate figure I showing distributions of $I$ for each agency 
# with two access functions

# log base 2
l2 = function(x){log(x,2)} 
# calculate I on a given vector where graphs and all that are already defined
mymoran = function(x){
	mi = moran(
		x=x,
		listw=nb2listw(nb,style='W'),
		n=length(od),
		S0=length(od)
	)
	return(mi$I)
}

# database connection
library('RPostgreSQL')
library('postGIStools')
con <- dbConnect( PostgreSQL(), dbname='diss', user='nate', host='localhost', password='mink')
# moran
library('spdep')

# main loop
x = list()
j = 1
for( agency in c('TTC','JTA','MBTA','Muni')){
	# get the data
	load(paste0('~/dissdata/R/',agency,'_A.RData'))
	# grab OD points from the DB
	od_table = switch(agency,'TTC'='ttc_od','JTA'='jv_od','Muni'='muni_od','MBTA'='mbta_od')
	od <- get_postgis_query(con,paste('SELECT * FROM',od_table,'ORDER BY uid'),geom_name='loc_geom')
	# Sphere of Influence Graph as defined here: 
	nb = graph2nb(soi.graph(tri2nb(od), coordinates(od)))
	plot.nb(nb,coordinates(od))
	for(func in c('negexp','cum')){
		param = switch(func,'cum'='45',negexp='30')
		eAo = A[['retro',func,param,'A_oh']] / A[['sched',func,param,'A_oh']]
		x[[j]] = apply(l2(eAo),2,mymoran)
		#x[j] = quantile(i_h,c(0,.25,.5,.75,1))
		j = j + 1
	}
}
dbDisconnect(con)

library('beeanplot')
beanplot(x,horizontal=T,method='stack',side='second',cut=1,bw=0.02,col=c(adjustcolor('red',alpha=.2)))


# set up the plot
y = 1:8
plot( x=c(x[,1],x[,3],x[,5]), y=c(y,y,y) )
segments( x0=x[,2],x1=x[,4],y0=y,y1=y )
abline(lm(i~l2(eAh)))

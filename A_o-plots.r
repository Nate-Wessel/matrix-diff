library('ks')
figures_dir = '/home/nate/Dropbox/diss/access/paper/figures/'
# log base 2
l2 = function(x){log(x,2)}
# database connection
library('RPostgreSQL')
library('postGIStools')
con <- dbConnect( PostgreSQL(), dbname='diss_access', user='nate', host='localhost', password='mink')
# moran
library('spdep') # moran
library('dplyr') # ntile
#
### PLOT THE A_o & A_oh FIGURES for all combinations
for( agency in c('TTC','JTA','MBTA','Muni')){
	print(agency)
	# get the data
	load(paste0('~/dissdata/access/R/',agency,'_A.RData'))
	for(type in c('negexp','cum')){
		param = switch( type,
			'cum'=switch(agency,'Muni'='30','45'),
			'negexp'=switch(agency,'Muni'='20','30') )
		print(type)
		# median error 
		e_oh = A[['retro',type,param,'A_oh']] / A[['sched',type,param,'A_oh']]
		y <- l2(apply( e_oh, 1, median ))
		# median A
		x <- apply( A[['sched',type,param,'A_oh']], 1, median )
		z <- apply(e_oh,1,IQR) # attribute displayed by color
		colors = rainbow(4,alpha=.5)
		colors = colors[ntile(z,4)]
		names(colors) <- names(x)
		# grab OD points from the DB
		od_table = switch(agency,'TTC'='ttc_od','JTA'='jv_od','Muni'='muni_od','MBTA'='mbta_od')
		od <- get_postgis_query(con,paste('SELECT * FROM',od_table,'ORDER BY uid'),geom_name='loc_geom')
		# Sphere of Influence Graph as defined here: 
		nb = graph2nb(soi.graph(tri2nb(od), coordinates(od)))
		# print I value
		print(moran(x,listw=nb2listw(nb,style='W'),n=length(od),S0=length(od))$I)
		cairo_pdf(paste0(figures_dir,agency,'-A_o-',type,'.pdf'),width=4.5,height=4.5)
			par( mai=c(.5,.6,0.1,.1), family='serif',bty='n' ) # bottom, left, top, right
			plot( x=x, y=y, col=colors, pch=19, yaxt='n', ylab='',xaxt='n',xlab='' )
			abline(h=median(y),v=median(x),col='grey',lty=2) # metamedians
			# add log-scale axis
			marks = quantile(y,c(0,.1,.5,.9,1))
			axis( 2, at=marks, labels=sprintf('%+.0f%%',(2^marks-1)*100), las=2, 
				pos=min(x)-diff(range(x))/30 )
			# x axis
			marks = quantile(x,c(0,.1,.5,.9,1))
			axis( 1, at=marks, labels=sprintf('%.0f%%',marks*100), las=0 )
			# add agency name in lower right corner
			text(x=par('usr')[2],y=par('usr')[3],labels=agency,adj=c(1.1,-.5),cex=2)
			if(exists('origins')){ # highlight selected zones
				for(O in origins){ # foreach selected origin
					if(agency==O$agency){
						# circled 
						points( x=x[O$o], y=y[O$o], pch=1, cex=2.5, lwd=2 )
						# labelled with letter
						text( x=x[O$o], y=y[O$o], labels=O$name, col='black', cex=1.5, pos=4 )
					}
				}
			}
		dev.off()
		#if(agency=='JTA'){4*'y'}
	}
}

# clean up
dbDisconnect(con)
#remove(type,param,e_oh,y,x,colors,O)
gc()

#agency='TTC'
#load(paste0('~/dissdata/access/R/',agency,'_A.RData'))
#type='negexp'
#param = switch( type,
#	'cum'=switch(agency,'Muni'='30','45'),
#	'negexp'=switch(agency,'Muni'='20','30') )
#e_oh = A[['retro',type,param,'A_oh']] / A[['sched',type,param,'A_oh']]
#y <- apply( e_oh, 1, median )
#write.csv(x=(y-1)*100,'/home/nate/ttc-negexp.csv')

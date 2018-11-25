library('ks')
figures_dir = '/home/nate/Dropbox/diss/paper/figures/'
# log base 2
l2 = function(x){log(x,2)} 
#
### PLOT THE A_oh FIGURES
for( agency in c('TTC','JTA','MBTA','Muni')){
	print(agency)
	# get the data
	load(paste0('~/dissdata/R/',agency,'_A.RData'))
	for(type in c('cum','negexp')){
		param = switch( type,
			'cum'=switch(agency,'Muni'='30','45'),
			'negexp'=switch(agency,'Muni'='20','30')
		)
		print(param)
		sched = A[['sched',type,param,'A_oh']]
		e = A[['retro',type,param,'A_oh']] / A[['sched',type,param,'A_oh']]
		# measures to be plotted
		standard_error = apply(e,1,sd)
		mean_error = exp(apply(l2(e),1,mean))
		# this is where we select a few origins for closer inspection
		# print their id along with the agency name
#		print( sort(standard_error) )
		# 
		pdf(paste0(figures_dir,agency,'-A_oh-',type,'.pdf'),width=5.5,height=5.5)
			par( mai=c(.8,.8,0.1,.1), pch='+', family='serif' ) # bottom, left, top, right
			plot( x = standard_error, y = mean_error, col=rgb(0,0,0,alpha=.5))
			abline(h=median(mean_error),v=median(standard_error),col='red')
			# add agency name in top right corner
			text(x=par('usr')[2],y=par('usr')[4],labels=agency,adj=c(1.1,1.3),cex=2)
			if(exists('origins')){ # highlight selected zones
				# TODO finish this
				for(O in origins){ # foreach selected origin
					if(agency==O$agency){
						# circled in red
						points( x=standard_error[O$o], y=mean_error[O$o], 
							pch=20, col='red',cex=2 )
						# labelled with letter
						text( x=standard_error[O$o], y=mean_error[O$o], 
							labels=O$name, col='red', cex=1.5, pos=2 )
					}
				}
			}
			dev.off()
		}
}

# clean up
#remove(type,'T',e,sched,mean_error,standard_error,param)
gc()

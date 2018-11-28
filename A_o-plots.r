library('ks')
figures_dir = '/home/nate/Dropbox/diss/paper/figures/'
# log base 2
l2 = function(x){log(x,2)}
#
### PLOT THE A_o & A_oh FIGURES for all combinations
for( agency in c('TTC','JTA','MBTA','Muni')){
	print(agency)
	# get the data
	load(paste0('~/dissdata/R/',agency,'_A.RData'))
	for(type in c('negexp','cum')){
		param = switch( type,
			'cum'=switch(agency,'Muni'='30','45'),
			'negexp'=switch(agency,'Muni'='20','30') )
		print(param)
		# median error 
		e_oh = A[['retro',type,param,'A_oh']] / A[['sched',type,param,'A_oh']]
		y = l2(apply( e_oh, 1, median ))
		# median A
		x = apply( A[['sched',type,param,'A_oh']], 1, median )
		colors = rainbow(4,alpha=.5)[ntile(apply(e_oh,1,IQR),4)]
		names(colors) <- names(x)
		pdf(paste0(figures_dir,agency,'-A_o-',type,'.pdf'),width=5.5,height=5.5)
			par( mai=c(.8,.6,0.1,.1), pch='+', family='serif',bty='n' ) # bottom, left, top, right
			plot( x=x, y=y, col=colors, pch=19, yaxt='n', ylab='' )
			abline(h=median(y),v=median(x),col='grey',lty=2) # metamedians
			# add log-scale axis
			marks = quantile(y,c(0,.5,1))
			axis( 2, at=marks, labels=sprintf('%+.0f%%',(exp(marks)-1)*100), las=2, 
				pos=min(x)-diff(range(x))/30 )
			# add agency name in lower right corner
			text(x=par('usr')[2],y=par('usr')[3],labels=agency,adj=c(1.1,-.5),cex=2)
			if(exists('origins')){ # highlight selected zones
				for(O in origins){ # foreach selected origin
					if(agency==O$agency){
						# circled 
						points( x=x[O$o], y=y[O$o], pch=1, 
							col='black', cex=3, lwd=2 )
						# labelled with letter
						text( x=x[O$o], y=y[O$o], labels=O$name, 
							col='black', cex=1.5, pos=4 )
					}
				}
			}
		dev.off()
		#if(agency=='Muni'){4*'y'}
	}
}

# clean up
#remove(type,param,e_oh,y,x,colors,O)
gc()

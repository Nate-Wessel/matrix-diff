source('~/matrix-diff/access-functions.r')
library('zoo')

# bandwidth parameters to calculate
params = seq(5,60,5)

A = array(list(NA),c(2,2,length(params),3))
dimnames(A)[[1]] = c('sched','retro')
dimnames(A)[[2]] = c('cum','negexp') # function names
dimnames(A)[[3]] = params
dimnames(A)[[4]] = c('A_h','A_oh','A_ot') # aggregations

# for each of these access functions
for(func_name in dimnames(A)[[2]] ){
	for(param in params){ # for each parameter
		for(dataset in dimnames(A)[[1]]){
			p = as.character(param)
			# get the function from the name
			print( paste('working on',dataset,func_name,p) )
			acc_func = get(func_name)
			# calculate momentary accessibility A_ot
			A_ot = apply( 
				acc_func( switch(dataset,'retro'=r_odt,'sched'=s_odt), param ), 
				c(1,3), sum, na.rm=T 
			) / dim(s_odt)[1]
			A[[dataset,func_name,p,'A_ot']] = A_ot
			# hour binned A_o scores
			A_oh = t(rollapply(t(A_ot),width=60,by=60,mean))
			A[[dataset,func_name,p,'A_oh']] = A_oh
			# hour-binned A scores
			A[[dataset,func_name,p,'A_h']] =  apply(A_oh,2,mean)
			# this is but shouldn't be necessary
			gc() 
		}
	}
}
remove(acc_func,func_name,param,params,p,A_ot,A_oh)
gc()

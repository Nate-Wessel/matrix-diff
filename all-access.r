source('access-functions.r')

A_results = array(NA,c(6,3,2))
dimnames(A_results)[[1]] = c('bin45','bin60','bin90','gauss30','gauss45','gauss60') # function names
dimnames(A_results)[[2]] = c('cor_ot','cor_o','retro/sched') # correlation measures
dimnames(A_results)[[3]] = c('workforce/jobs','zones/zones') # weighting schemes

for( weighting_scheme in dimnames(A_results)[[3]] ){
	print(weighting_scheme)
	w_names = strsplit(weighting_scheme,'/')[[1]]
	w1 = w_names[1]
	w2 = w_names[2]
	# for each of these access functions
	for( func_name in dimnames(A_results)[[1]] ){
		print( func_name )
		func = get(func_name)
		# do the accessibility calculations
		s_acc = access( s_odt, od@data[,w1], od@data[,w2], func )
		r_acc = access( r_odt, od@data[,w1], od@data[,w2], func )
		# pull out the measures and put them in the result matrix
		A_results[func_name,'cor_ot',weighting_scheme] =  cor( c(s_acc[[3]]), c(r_acc[[3]]) )
		A_results[func_name,'cor_o',weighting_scheme] = cor( s_acc[[2]], r_acc[[2]] )
		A_results[func_name,'retro/sched',weighting_scheme] = r_acc[[1]] / s_acc[[1]] 
	}
}
remove(func,func_name,weighting_scheme,w1,w2,w_names)


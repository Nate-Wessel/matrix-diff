source('access-functions.r')

A = array(list(NA),c(2,3,3))
dimnames(A)[[1]] = c('retro','sched')
dimnames(A)[[2]] = c('cum','gauss','negexp') # function names
dimnames(A)[[3]] = c('A','A_o','A_ot') # aggregations

# set the weights
w1 = w2 = 'zones'
# for each of these access functions
for( func_name in dimnames(A)[[2]] ){
	# get the function from the name
	print( func_name )
	func = get(func_name)
	# do the accessibility calculations
	s_acc = access( s_odt, od@data[,w1], od@data[,w2], func )
	r_acc = access( r_odt, od@data[,w1], od@data[,w2], func )
	# pull out the measures and put them in the result matrix
	A[['retro',func_name,'A']] =  r_acc[[1]]
	A[['sched',func_name,'A']] =  s_acc[[1]]
	A[['retro',func_name,'A_o']] = r_acc[[2]]
	A[['sched',func_name,'A_o']] = s_acc[[2]]
	A[['retro',func_name,'A_ot']] = r_acc[[3]]
	A[['sched',func_name,'A_ot']] = s_acc[[3]]
}

remove(func,func_name,weighting_scheme,w1,w2,w_names)


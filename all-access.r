source('~/matrix-diff/access-functions.r')

A = array(list(NA),c(2,2,3))
dimnames(A)[[1]] = c('sched','retro')
dimnames(A)[[2]] = c('cum','negexp') # function names
dimnames(A)[[3]] = c('A','A_o','A_ot') # aggregations

# set the weights
w1 = w2 = 'zones'
# for each of these access functions
for( func_name in dimnames(A)[[2]] ){
	# get the function from the name
	print( func_name )
	func = get(func_name)
	# do the accessibility calculations
	s_acc = access( s_odt, func )
	r_acc = access( r_odt, func )
	# pull out the measures and put them in the result matrix
	A[['retro',func_name,'A']] =  r_acc[[1]]
	A[['sched',func_name,'A']] =  s_acc[[1]]
	A[['retro',func_name,'A_o']] = r_acc[[2]]
	A[['sched',func_name,'A_o']] = s_acc[[2]]
	A[['retro',func_name,'A_ot']] = r_acc[[3]]
	A[['sched',func_name,'A_ot']] = s_acc[[3]]
}
remove(func,func_name,w1,w2,s_acc,r_acc)


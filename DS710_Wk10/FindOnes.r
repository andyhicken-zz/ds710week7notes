FindOnes <- function(x){
	# Extracts rows of x with 1's in columns 10-12
	y = NULL
	num.rows = dim(x)[1]
	for( i in 1:num.rows ){
		num.ones = length(which( x[i, 10:12] == 1 ))
		if(num.ones > 0){
			y = rbind( y, x[i,] )
		} # end "if there are ones"
	} # end iteration over rows of x
	return(y)
} # end function FindOnes

FindOnes.Fast <- function(x){
	# Extracts rows of x with 1's in columns 10-12
	y = x
	j = 1
	num.rows = dim(x)[1]
	for( i in 1:num.rows ){
		num.ones = length(which( x[i, 10:12] == 1 ))
		if(num.ones > 0){
			y[j,] = x[i,]
			j = j + 1
		} # end "if there are ones"
	} # end iteration over rows of x
	return(y[1:(j-1),])
} # end function FindOnes.Fast

## Matrix Inversion is a time-consuming process. Given a matrix A, we need to
## find another matrix B, such that, when A and B are multiplied, we get an
## identity matrix. An Identity matrix has 1's across the main diagonal from
## top-left to bottom-right and all others are 0.
##
## In this assignment, we are trying to cache the inverse matrix, so we do not
## have to keep re-computing it (again since it's an expensive activity)


## This function is just a wrapper, it does not do any real matrix inversion
## It stores and return either the matrix itself or it stores and returns the 
## inverse
makeCacheMatrix <- function(A = matrix()) {
	# Matrix A is the input matrix
	# Matrix B is A's inverse
	B <- matrix() #initialize to empty 
	
	# To call this - first you need a matrix
	#   A <- matrix(1:16,4,4)
	# then call it
	#   X <- makeCacheMatrix(A)
	# Above call goes to set()
	set <- function(x) {
			A <<- x
			B <<- matrix() #at the beginning, when A is set, we do not know the inverse B
	}

	# get() returns what was set
	get <- function() A

	# setinverse() saves the inverse B
	setinverse <- function(inverse) B <<- inverse #save the inverse
	
	# getinverse() returns the saved inverse B
	getinverse <- function() B #return the inverse
	
	# list() just returns functions in this makeCacheMatrix function
	list(set = set, 
		 get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}

## This function checks first to see if the matrix inverse is already available
## If yes, it just retrieves it and returns it
## If not, it computes the inverse using solve(), stores the inverse and 
##         returns it.
## Just to make sure that the function is working, there is a message when 
## inverse is found in cache
cacheSolve <- function(A, ...) {
	#let's try to get B first
	B <- A$getinverse()
	if(!all(is.na(B))) { #check if it not all NA's, if so, we have the inverse already - return it
			message("Getting cached data")
			return(B)
	}
	# we don't have teh inverse B in our cach
	tmp <- A$get()
	B <- solve(tmp, ...) #just use solve to find inverse
	A$setinverse(B) #set the inverse for next time and return the inverse
	B
}

## I used the following to test above
## n <- 10; A <- matrix(rnorm(n*n),n,n); X <- makeCacheMatrix(A); cacheSolve(X); cacheSolve(X)
## n <- 4; A <- matrix(rnorm(n*n),n,n); X <- makeCacheMatrix(A); B <- cacheSolve(X); B %*% A
## Known Failure: If matrix is singular, then it's inverse is not defined - you get an error for this.
## I don't know enough R to determine how to catch that exception from Lapack.

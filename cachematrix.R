## Put comments here that give an overall description of what your
## functions do
## ASSIGNMENT #2. SB
## ------------------------------------------------------------------
## Write a short comment describing this function
## This function creates a special "matrix" OBJECT that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	## Initial declaration
	invMatrix <- NULL
	
	## SET function
	set <- function( matrix )
	{	tempMatrix <<- matrix
		invMatrix <<- NULL ## Initialize
	}
	
	## INVERSE
	setInverse <- function(inverse)
	{	invMatrix <<- inverse
	}
	
	## GET function
	get <- function()
	{	tempMatrix
	}
	
	## GET INVERSE function
	getInverse <- function()
	{	invMatrix
	}
	
	list(set = set, get = get,
	setInverse = setInverse,
	getInverse = getInverse)
}
## ------------------------------------------------------------------
## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
## If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	tempMatrix <- x$getInverse()
	
	if(!is.null(tempMatrix))
	{	## message("DBG1: getting tempMatrix data")
		return(tempMatrix)
	}
	
	data <- x$get()
	## INVERSE: using Matrix multiplication
	tempMatrix <- solve(data) %*% data
	## STORE the INVERSE
	x$setInverse(tempMatrix)
	## Return
	tempMatrix
}

## This assignment is to write a pair of functions that cache the
## inverse of a matrix. Functions: makeCacheMatrix and cacheSolve

## Function makeCacheMatrix creates a list of 4 functions:
##  set() - sets the values of the matrix
##  get() - gets the values of the matrix
##  setinv() - sets the values of the inverse of the matrix
##  getinv() - gets the values of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinv <- function(inv) inverse <<- inv
	getinv <- function() inverse
	
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...) {
	mat_inv <- x$getinv()
	if (!is.null(mat_inv)) {
			message ("Getting cached inverse matrix")
			return(mat_inv)		
	}

	my_mat <- x$get()
	mat_inv <- solve(my_mat, ...)
	x$setinv(mat_inv)
	mat_inv
	
	
}


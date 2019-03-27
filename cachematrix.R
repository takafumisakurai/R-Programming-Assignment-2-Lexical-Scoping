
## The functions are used to compute the inverse of a matrix.

## makeCacheMatrix creates a list containing functions to
	# set the value of the matrix
	# get the value of the matrix
	# set the value of the inverse matrix
	# get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse)  inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve calculates the inverse of the special "matrix" created by makeCacheMatrix. 
# cacheSolve first checks to see if the inverse of the matrix has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function. 
cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	matrix <- x$get()
	inv <- solve(matrix, ...)
	x$setInverse(inv)
	inv
}

## This function is a cache function that caches the resulting inverse
## of an invertible matrix.

## makeCacheMatrix() returns a fully formed object of type makeCacheMatrix() to be used by the next function -- cacheSolve()
	inv <<- NULL
	set <- function(y) {
			x <<- y
			inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inverse of an invertible matrix. If the matrix is already in the Cache function above, then the inverse can be easily retrieved.
## It is important too note that cacheSolve requires and argument type of makeCacheMatrix().
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse()
		if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$setInverse(inv)
		inv
}

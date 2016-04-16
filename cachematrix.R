## These functions provide a means to create a matrix
#  object that can cache its inverse.

## This returns a matrix object with four associated setter/getter
#  functions enabling the caching of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## This function calculates the inverse of the given matrix
#  or if possible returns a cached result.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
		if (!is.null(inv)) {
			message("Using cached data")
			return(inv)
		}
		data <- x$get()
		inv <- solve(data)  # Assuming matrix is square
		x$setInverse(inv)
		inv
}
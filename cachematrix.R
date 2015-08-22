## This file contains two functions makeCacheMatrix & cacheSolve
## These functions are used to create matrix with cacheable iverse
## 
## Example Usage:
## cacheable <- makeCacheMatrix(matrix(rnorm(9), ncol = 3, nrow = 3))
## cacheSolve(cacheable)


## makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse.
## Argumens: x - matrix
## Return: cacheable matrix
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by
## makeCacheMatrix function. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.
## Inverse of a square matrix is done with the solve function.

## Argumens: x - cacheable matrix object
## Return: inverse matrix
cacheSolve <- function(x, ...) {
	cachedInverse <- x$getInverse()
	if(!is.null(cachedInverse)) {
        message("returning cached data")
        return(cachedInverse)
	}
	matrix <- x$get()
	calculatedInverse <- solve(matrix, ...)
	x$setInverse(calculatedInverse)
    calculatedInverse
}

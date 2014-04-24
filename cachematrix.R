# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly. This script implement a pair of functions that
# cache the inverse of a matrix.

# This function creates a special "matrix" wrapper object
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    # variable to store the cached inverse matrix
    inverse <- NULL
    
    # set the actual matrix inside the wrapper
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # get the actual matrix
    get <- function() x
    
    # set the cached inverse matrix
    setinverse <- function(inv) inverse <<- inv
    
    # get the cached inverse matrix
    getinverse <- function() inverse
    
    # construct the list of inner functions
    list(set = set, get = get,
         getinverse = getinverse,
         setinverse = setinverse)
}

# This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

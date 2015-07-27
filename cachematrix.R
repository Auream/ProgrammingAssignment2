## The following two functions (makeCacheMatrix and cacheSolve) can be used to
## cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    set <- function(y) {
        x <<- y
        inv_m <<- NULL
    }
    get <- function() x
    setinverse <- function(inversed) inv_m <<- inversed
    getinverse <- function() inv_m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_m <- x$getinverse()
    if(!is.null(inv_m)){
        message("getting cached data")
        return(inv_m)
    }
    data <- x$get()
    inv_m <- solve(data, ...)
    x$setinverse(inv_m)
    inv_m
}

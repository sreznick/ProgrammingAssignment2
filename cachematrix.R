## Functions for cached computation of inverted matrix
## It is assumed that matrix is invertible

## Create object representing matrix and its cached 
## inverted peer
makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverted <<- inv
    getinv <- function() inverted
    list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Uses instance created by makeCacheMatrix
## to obtain (calculating first time and using cached value next times)
## inverted matrix

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(is.null(inv)) {
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
    } else {
        message("getting cached data")
    }
    inv
}

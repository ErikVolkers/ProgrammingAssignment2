#######################################################################
## This script contains two functions:                              ###
##                                                                  ###
## 1. makeCacheMatrix: This function creates a special "matrix"     ###
## object that can cache its inverse. It is a list of 4 functions.  ###
##                                                                  ###
## 2. cacheSolve: This function computes the inverse of the special ###
## "matrix" returned by makeCacheMatrix above. If the inverse has   ###
## already been  calculated (and the matrix has not changed), then  ###
## the cachesolve retrieves the inverse from the cache.             ###                
#######################################################################

## Create the "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setmatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(setmatrix = setmatrix,
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$getmatrix()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    }




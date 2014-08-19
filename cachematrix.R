## cachematrix.R
## The following functions provide a way to return the inverse of a matrix 
## by caching the inverse and avoid costly computation of calculating it
## each time when using the functions repeatedly.


## Function: makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function: cacheSolve
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    ## Check if inverse was calculated and cached
    inv <- x$getinverse()
    if(!is.null(inv)) {
        ## Return previously calculated inverse
        message("Getting cached data")
        return(inv)
    }
        ## Calculate a matrix that is the inverse of 'x'
        message("Calculating and returning inverse")
        m <- x$get()
        inv <- solve(m, ...)
        x$setinverse(inv)
        return(inv)

}

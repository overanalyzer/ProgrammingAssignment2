## cachematrix.R:
## These functions implement a wrapper for a Matrix to enable caching of
## costly computations related to the matrix.


## function makeCacheMatrix:
## Takes a matrix, and creates a wrapper around it that provides access
## to the underlying matrix, as well as functions to get and set cached
## computations related to the matrix (namely inverse, for this implementation)
makeCacheMatrix <- function(x = matrix()) {
    ## default cached inverse to NULL, so we know whether we have it cached
    cachedInverse <- NULL
    
    ## sets the value of the matrix, x
    set <- function(value) {
        ## set x in the parent environment (the makeCacheMatrix function)
        x <<- value
        ## null the cached inverse, since the matrix has changed
        cachedInverse <<- NULL
    }
    
    ## gets the value of the matrix, x
    get <- function() x
    
    ## set the cached inverse 
    setInverse <- function(inverse) cachedInverse <<- inverse
    
    ## gets the cached inverse
    getInverse <- function() cachedInverse
    
    ## build and return list containing the functions
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## function cacheSolve:
## Takes a Cache Matrix object and returns the inverse of the underlying matrix
## by using solve, returning either a cached copy of the inverse (if available), or
## creating the inverse and caching it for future use.
## Assumes that the matrix is invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## get the inverse from the x object
    inverse <- x$getInverse()
    
    ## check if the inverse is set. if so, we have a cached copy of it.
    if (!is.null(inverse)) {
        message("using cached inverse")
        ## return cached copy of inverse
        return(inverse)
    }
    
    ## otherwise, we need to calculate the inverse and store it
    matrixData <- x$get()
    inverse <- solve(matrixData)
    x$setInverse(inverse)
    
    ## return calculated inverse
    message("new inverse calculated")
    inverse
}

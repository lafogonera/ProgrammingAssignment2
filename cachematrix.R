## cache the inverse of a matrix.  


## This function creates a special "matrix" object that can cache its inverse.
## It receives a matrix as a parameter.
## It stores/cache the inverse matrix in the 'inverse' property
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ## Set the matrix if we haven't done it on the creation of the makeCacheMatrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    calculateInverse <- function() {
        message("makeCacheMatrix / Calculating and storing the inverse of the matrix")
        inverse <<- solve(x)
    }
    getInverse <- function() {
        inverse
    }
    ## public exposure of methods
    list(set = set, get = get, calculateInverse = calculateInverse, getInverse = getInverse)
}


## Returns the inverse of a matrix stored in a makeCacheMatrix object
## gets as parameter a makeCacheMatrix
cacheSolve <- function(x, ...) {
    
    ## If inverse is NULL (only this case), calculate it and store it on 
    ## the makeCacheMatrix object
    if(is.null(x$getInverse())) {
        message("cacheSolve / Inverse is NULL. We must calculate it")
        x$calculateInverse()
    } 
    
    ## return the inverse of the matrix   
    return(x$getInverse())
}

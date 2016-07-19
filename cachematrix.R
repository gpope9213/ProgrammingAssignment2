## COURSERA Programming Assignment 2:
##
## Create two functions that: 
##      (1) creates a special "matrix" object that can cache its inverse 
##      (2) computes the inverse of the special "matrix" returned by function (1); 
##              if the inverse has already been calculated (and the matrix has not changed),
##              then the inverse should be retrieved from the cache                          
##
## Function (1) makeCacheMatrix: a function that creates a matrix that can be inverted
##
makeCacheMatrix <- function(x = matrix()) {
        ## variable for cached matrix inverse
        mtx <- NULL
        ## get and set functions
        set <- function(y) {
                x <<- y
                mtx <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) mtx <<- solve
        getInverse <- function() mtx
        ## returns the list of functions for the matrix
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
##
## Function (2) cacheSolve: computes the inverse of a matrix.  If the inverse has already been calculated, 
##              and the matrix is unchanged, then the cached inverse is returned. 
## 
cacheSolve <- function(x=matrix(), ...) {
        mtx <- x$getInverse()
        ## Return the cached inverse if it has already been computed
        if(!is.null(mtx)) { 
                message("cached data returned")
                return(mtx)
        }
        ## compute the inverse of the matrix
        matrix <- x$get()
        mtx <- solve(matrix, ...)
        ## cache the inverse
        x$setInverse(mtx)
        ## return the inverse of the matrix
        mtx
}

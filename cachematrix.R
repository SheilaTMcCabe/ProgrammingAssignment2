## OVERVIEW:
## The function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse. The cacheSolve function computes
## the inverse of the matrix. 

## ASSUMPTION: The matrix supplied is always invertible.

## This function makeCacheMatrix does the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize to NULL
        m <- NULL
        
        
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## set the value of the inverse of the matrix
        setsolve <- function(solve) m <<- solve
        
        ## get the value of the inverse of the matrix
        getsolve <- function() m
        
        ## Lists out the values of the functions in the makeCacheMatrix
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix function. It first checks to see if the
## inverse was calculated. If the inverse has already been calculated
## (and the matrix has not changed), then this cacheSolve function should
## retrieve the inverse from the cache. Otherwise, it calculates the inverse
## of the matrix and sets the value in the cache via the solve function in R.

cacheSolve <- function(x, ...) {
        
        ## Assigns inverse
        m <- x$getsolve()
        
        ## If the inverse has been evaluated before, then display
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        else {
                ## If the inverse has NOT been evaluated before, retrieve local variable
                data <- x$get()
                
                ## Calculate the inverse
                m <- solve(data, ...)
                
                ## Assign the calculated inverse and display
                x$setsolve(m)
                m 
        }
        
}

## Based on the templates provided in the second programming assignmentfor the R Programming
## course offered by Johns Hopkins University as part of the Data Science specialization 
## in Coursera

## creates a special "cache matrix" object, which is really a list with 4 elements
## and each element is a function:
## [1] set, to set the matrix to which the inverse is to be calculated
##     (this function also nullifies the inverse as it has not been calculated yet)
## [2] get, to get the matrix to which the inverse is to be (or has been) calculated
## [3] setsolve, to set the inverse matrix of the matrix
## [4] getsolve, to get the inverse matrix of the 

makeCacheMatrix <- function(x = matrix()) {
        
        s <- NULL  
        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() x
        
        setsolve <- function(solve) s <<- solve
        
        getsolve <- function() s
        
        ## construct the 'cache matrix', with the definition of the functions
        ## to set and get either the matrix or its inverse
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The cacheSolve function calculates the inverse of the special "cache matrix" 
## created with the above function, makeCacheMatrix. 
## Before actually calculating the inverse, createSolve checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the matrix 
## and sets the calculated inverse matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

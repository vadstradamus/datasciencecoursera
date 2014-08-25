# Matrix inversion is usually a costly computation 
# and their may be some benefit to caching the inverse
# of a matrix rather than compute it repeatedly (there 
# are also alternatives to matrix inversion that we 
# will not discuss here). Your assignment is to write
# a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
#     
# makeCacheMatrix: This function creates a special 
# "matrix" object that can cache its inverse.
#
# cacheSolve: This function computes the inverse of
# the special "matrix" returned by makeCacheMatrix 
# above. 
# 
# If the inverse has already been calculated 
# (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the 
# cache. Computing the inverse of a square matrix 
# can be done with the solve function in R. For 
# example, if X is a square invertible matrix, then
# solve(X) returns its inverse.

makeCacheMatrix <- function(x=matrix()) {
    
# Creates a special "mector", which is really
# a list containing a function to
#
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of the inverse
#   4. get the value of the inverse

    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    
    setsolve <- function(solve) s <<- solve

    getsolve <- function() s
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}

cacheSolve <- function(x, ...) {
    
    s <- x$getsolve()
    
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    matrix <- x$get()
    
    s <- solve(matrix, ...)
    
    x$setsolve(s)
    
    s
    
}

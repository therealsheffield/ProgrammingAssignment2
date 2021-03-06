#-------------------------------------------------------------------------------
# cachematrix.R
#
# Contains functions to represent an invertible matrix and its potentially
# cached inverse.
#
# Author: therealsheffield
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#
# makeCacheMatrix(x = matrix)
#
# Makes a matrix that will cache its inverse when it is calculated with the
# cacheSolve() function.
#
# Parameter x : an invertible matrix
# Returns list(set,get,setinverse,getinverse) representing a matrix with a
#                                             cacheable inverse and the
#                                             following functions:
#
#         set : a function that sets a new matrix and resets any cached inverse
#               to NULL
#         get : a function that returns the current matrix
#         setinverse : a function that sets, but does not calculate, the cached
#                      inverse
#         getinverse : a function that returns the cached inverse
#
#-------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    # Set the cache to NULL initially
    cinverse <- NULL

    # Define the set function. Sets the new matrix and empties the cache
    set <- function(newmatrix) {
        x <<- newmatrix
        cinverse <<- NULL
    }

    # Define the get function. Returns the matrix.
    get <- function() x

    # Define the setinverse function. Caches the new inverse.
    setinverse <- function(newinverse) cinverse <<- newinverse

    # Define the getinverse function. Returns the cached inverse.
    getinverse <- function() cinverse

    # Return the "matrix" which is really a list of the functions defined above.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#-------------------------------------------------------------------------------
#
# cacheSolve(x, ...)
#
# Returns the (potentially cached) inverse of a matrix. This function will
# compute the inverse of a matrix with solve(x,...) if it hasn't been computed
# yet. If the inverse has been cached that will be returned.
#
# Parameter x : an invertible matrix created with the makeCacheMatrix() function
# Parameters ... : extra parameters passed to the solve() function
# Returns the (potentially cached) inverse of x
#
#-------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
    # Get the cached inverse matrix
    cinverse <- x$getinverse()
    if (!is.null(cinverse)) {
        # Inverse was cached. Print a message and return it.
        message("getting cached data")
        return(cinverse)
    }
    # Inverse wasn't cached. Calculate the inverse, cache it, and return it.
    matrix <- x$get()
    cinverse <- solve(matrix, ...)
    x$setinverse(cinverse)
    cinverse
}

## Put comments here that give an overall description of what your
## functions do
# Functions presented in this file are used to create a special object that stores an matrix and cache its inverse. The first function creates the special
# matrix, while the second is in charge of computing the inverse and set the value in the cache (if it is not already in the cache)

## Write a short comment describing this function
# This function creates a special "matrix" that can cache its inverse. It is really a list containing a function to (1) set the value of the matrix, 
# (2) get the value of the matrix, (3) set the value of the inverse of the matrix, and (4) get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
# This function calculates the inverse of the special "matrix" created with the function "makeCacheMatrix". If first checks if the inverse has been already
# calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value in
# the cache via the setInverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}

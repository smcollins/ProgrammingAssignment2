## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# the makeCacheMatrix creates a special "vector" which is really a list containing
# a function to:
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
    
}


## Write a short comment describing this function
# cacheSolve calculates the inverse of a given matrix (the "special vector")
# created in the above function. It first checks to see if the inverse has
# already been calculated and gets the inverse instead of computing it, if available.
# Otherwise it will calculate and setInverse in the cache.

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

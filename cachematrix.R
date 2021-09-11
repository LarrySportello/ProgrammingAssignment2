## The purpose of these two functions (to be used in conjunction) calculates the inverse of a matrix, stores that output in a cache variable, and will return the output from cache (without recalculating) if the inverse function is calculated again on the same matrix

## makeCacheMatrix creates a list of functions based on a given matrix input, the output of this function should be assigned to a unique variable for each unique matrix
## that output variable should then be passed to cacheSolve


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve take the variable-stored output of makeCacheMatrix as its input, and calculates the inverse of the matrix, or uses cache to recall the inverse if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

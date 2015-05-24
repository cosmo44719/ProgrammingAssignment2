## A pair of functions to cache the result
## of the inversion of a matrix

## makeCacheMAtrix creates a matrix object
## to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        Inverse <- NULL
        set <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) Inverse <<- solve
        getinverse <- function() Inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix
## returned by makeCacheMatrix. If the inverse
## has been already calculated it retrives it from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                Inverse <- x$getinverse()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        data <- x$get()
        Inverse <- solve(data, ...)
        x$setinverse(Inverse)
        Inverse
}

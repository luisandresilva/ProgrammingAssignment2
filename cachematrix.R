## The two functions below are intended to save computation time
## after the calculation of an inverse matrix
## The calculated inverse is available in cache for future use

## the function below maintains the cache of a calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) s <<- inverse
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## the function below returns a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if (!is.null(s)) {
            message("getting cached data")
            return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setinverse(s)
        s
}

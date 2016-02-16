## The makeCacheMatrix and cacheSolve functions are designed to provide
## a convenient way of storing a matrix along with a cache of its inverse.

## makeCacheMatrix(x) returns a list of functions to set/get
## the matrix x, and set/get the inverse of x (setinverse/getinverse)
## x is assumed to be an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    mInverse <- NULL
    set <- function(y) {
        x <<- y
        mInverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) mInverse <<- inverse
    getinverse <- function() mInverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve(x) returns a matrix that is the inverse of x, where x
## is a "matrix list" returned by the makeCacheMatrix function above. 
## x$get is assumed to return an invertible matrix.

cacheSolve <- function(x, ...) {
    mInverse <- x$getinverse()
    if (!is.null(mInverse)) {
        message ("getting cached data")
        return(mInverse)
    }
    data <- x$get()
    mInverse <- solve(data,...)
    x$setinverse(mInverse)
    mInverse
}

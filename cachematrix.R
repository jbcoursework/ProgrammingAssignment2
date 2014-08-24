## Functions to cache the result of matrix inversion
## The functions use a matrix object capable of caching its inverse

## makeCacheMatrix creates a special matrix object which is a list containing
## the matrix and potentially its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setinverse <- function(i) {
        inv <<- i
    }
    getinverse <- function() inv
    return(list(set=setinverse, get=getinverse, matrix=x))
}


## cacheSolve returns the inverse of the matrix contained in x. x must be a
## special matrix object returned from makeCacheMatrix. If cacheSolve is called
## mutliple times it returns the previously calculated inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get()
    if (!is.null(i)) {
        return(i)
    }
    i <- solve(x$matrix)
    x$set(i)
    return(i)
}

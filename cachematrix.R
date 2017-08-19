## The aim of these functions is to optimize processing while computing matrices. Instead of computing them
## repeatedly, they cache the inverse of a matrix, very useful when dealing with a large amount of data/matrices.

## This first function creates a special matrix object and caches its inverse. The elements are cached so that
## the other functions can access the object, using lexical scoping (storing in the Global Environment)

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function computes the inverse of the special matrix returned by the previous function
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
                message ("retrieving cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}

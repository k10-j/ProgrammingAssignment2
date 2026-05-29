## These two functions cache the inverse of a matrix so that it is
## computed once and retrieved from cache on subsequent calls.

## makeCacheMatrix creates a special "matrix" object that stores a
## matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve returns the inverse of the matrix stored in the special
## object. If the inverse is already cached, it returns the cached value;
## otherwise it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
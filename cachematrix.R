## Functions to extend and work with matrices that have an internal cache.
## The cache can store results of expensive calculations, e.g. the inverse
## of the matrix.

## makeCacheMatrix(x) wraps the given matrix 'x' in a new object that has an
## internal cache. The object has setter and getter functions for the matrix
## and the inverse of the matrix. Setting a new matrix clears the cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() { x }
    setinv <- function(inverse) { inv <<- inverse }
    getinv <- function() { inv }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve(x) returns the inverse of tne cacheMatrix 'x . On the first call,
## the inverse matrix is calculated and stored in the cache. Subsequent calls
## return the cached value.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinv(inv)
    inv
}

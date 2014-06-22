## Put comments here that give an overall description of what your
## functions do
## These functions are meant to provide the inverse of
## an invertible square matrix doing the calculation only one time.
## From the second time the cached inverse matrix is returned.
## The goal here is to save computer resources when the matrix must
## be inverted repeatedly.

## Write a short comment describing this function
## Similar to "makeVector", this function creates a special "matrix" which is
## really a list containing gets/sets for the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setimatrix <- function(solve) i <<- solve
    getimatrix <- function() i
    list(set = set, get = get,
         setimatrix = setimatrix,
         getimatrix = getimatrix)
}

## Write a short comment describing this function
## Similar to "cachemean", this function returns the inverse of 
## x using "Solve()" or get the inverse from the cache,
## if already been calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getimatrix()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setimatrix(i)
    i
}

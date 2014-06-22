## Matrix inversion is a costly computation and there often can be
## some benefit to caching the inverse of a matrix rather than re-computing it
## repeatedly.  This script contains two functions that will cache the inverse
## of a matrix.  We assume that the matrix supplied is alawys invertible.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inverseX <- NULL
     set <- function(y) {
          x <<- y
          inverseX <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) inverseX <<- inverse
     getInverse <- function() inverseX
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix' above. If the inverse has already been calculated (and the 
## matrix has not changed), then `cacheSolve` should retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inverseX <- x$getInverse()
     if(!is.null(inverseX)){
          message("Getting cached data")
          return(inverseX)
     } else{
     inverseX <- solve(x$get(), ...)
     x$setInverse(inverseX)
     return(inverseX)
     }
}

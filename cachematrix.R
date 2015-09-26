## Provides a wrapper for a matrix and its inverse. This allows 
## the inverse to be cached and referenced later without having
## to be recomputed. 

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  
  ## Sets the matrix x
  set <- function(y) {
    x <<- y
    xInverse <<- NULL
  }
  
  ## Gets the matrix x
  get <- function() x
  
  ## Sets the inverse of x
  setInverse <- function(inverse) xInverse <<- inverse
  
  ## Gets the inverse of x
  getInverse <- function() xInverse
  
  ## Function list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverse of a matrix. This function first checks
## to see if the inverse exists in the cache. If so, it will return
## the cached value. If not, it will solve for the inverse, cache it,
## and return the inverse. 

cacheSolve <- function(x, ...) {
  xInverse <- x$getInverse()
  
  if (is.null(xInverse)) {
    ## The inverse doesn't exist in the cache.
    ## We have to compute it. 
    xInverse <- solve(x$get(),...)
    x$setInverse(xInverse)
  }
  
  xInverse
}

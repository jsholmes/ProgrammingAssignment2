## Pair of functions that can be used to cache the inverse of
## a matrix without recalculating it

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() { x }
  cacheinverse <- function(inv) { inverse <<- inv }
  getinverse <- function() { inverse }
  list(set = set, get = get, 
       cacheinverse = cacheinverse, getinverse = getinverse)
}


## Computes the inverse of a matrix createad by makeCacheMatrix
## or if the inverse was already computed, returns the cached result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cached_inv <- x$getinverse()
  if (!is.null(cached_inv)) {
    message("getting cached data")
    return(cached_inv)
  }
  X <- x$get()
  inverse <- solve(X)
  x$cacheinverse(inverse)
  inverse
}

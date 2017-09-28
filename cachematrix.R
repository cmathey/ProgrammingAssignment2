## Put comments here that give an overall description of what your
## functions do


## Create a wrapper for caching the inverse of a matrix
## The wrapper provides functions to set/get the wrapped matrix and retrieve/store its inverse from the cache
makeCacheMatrix <- function(x = matrix()) {
  inv_ <- NULL  # the cached inverse
  set <- function(y) {
    x <<- y
    inv_ <<- NULL # invalidate cache
  }
  get <- function() x
  setinv <- function(inv) inv_ <<- inv
  getinv <- function() inv_
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculates the inverse of the cache matrix wrapper 
## In case of a cache miss, actually compute the inverse, otherwise return the cached value
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) { # cache hit
    message("reuse inverse from cache")
    return(inv)
  }
  # cache miss
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv) # cache for future use
  invx
}

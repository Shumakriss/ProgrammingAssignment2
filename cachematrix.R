## cacheMatrix.R
## Chris Shumaker
## R Programming, Coursera
## Assignment 2
## Provides two functions for caching the inverse of a matrix.
##
## Usage: First, call makeCacheMatrix on a normal invertible R matrix.
## This returns a list of functions you can call relating to that matrix.
## Second, call cacheSolve on the newly created cached matrix to return it's inverse.
##
## Assumptions: All matrix parameters are invertible

## makeCacheMatrix accepts an invertible square R matrix and returns a list of functions pertaining to it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Cache solve takes a cache matrix and returns the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

## This file contains the functions makeCacheMatrix(x) and cacheSolve(a)
##
## makeCacheMatrix(x) takes a matrix x and returns a list containing functions to
## read and write the matrix and its inverse, allowing the latter to be stored
##
## cacheSolve takes a list created with makeCacheMatrix(x) and returns the inverse
## of the matrix x, either by computing it or by reading it from the cache (if it
## has already been computed before)

## The functions inside makeCacheMatrix are elements of the list generated, and can
## therefore be called using "$". Example:
## a <- makeCacheMatrix(matrix(rexp(100),10))
## a$get()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes the list generated with make CacheMatrix as argument. It can also
## contain extra arguments to be used in the call to solve()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

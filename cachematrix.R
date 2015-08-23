## makeCacheMatrix creates a special matrix that can cache
## its inverse.  It creates a list that contains functions to
##    1. set the matrix
##    2. get the matrix
##    3. set the inverse
##    4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse
  getinverse = function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the inverse of the matrix.  It checks to
## see if the inverse has been computed.  If cache is computed,
## it will skip computation and give out the result.  If not,
## it will complete the inverse and give out a value based
## on the setinverse function.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrixdata <- x$get()
  inv <- solve(matrixdata, ...)
  x$setinverse(inv)
  return(inv)
}

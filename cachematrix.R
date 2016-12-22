#Writen a pair of functions that cache the inverse of a matrix.

## "makeCacheMatrix" fnction creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    #inverse <<- NULL
  }
  get <- function()  x 
  setInverse <- function(matrixInverse)  inverse <<- matrixInverse 
  getInverse <- function() inverse 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## "cacheSolve" function calculates the inverse of a square matrix returned by makeCacheMatrix.
## First checks in cache to see if the inverse has already been calculated (if the input variable name is same).
## Otherwise, it calculates the inverse of the matrix and sets the value in cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Retrieving cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  return(inverse)
  }

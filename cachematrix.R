## A pair of functions that cache the inverse of a matrix


## This function takes in a square inverible matrix and returns a list of
## functions used as the input to the following function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function takes the output of the previous function and returns
## the inverse of the original matrix input (x)

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  inv <- x$get()
  m <- solve(inv, ...)
  x$setmatrix(m)
  return(m)
}

## Put comments here that give an overall description of what your
## functions do

## create 'object' that hold matrix and cached value associated with this matrix
## (by name convetion this value is an inversion of matrix)
## setting new matrix on that object, also resets associated value
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(newInv) inv <<- newInv
  getinv <- function() inv
  
  list( set = set, get = get, setinv = setinv, getinv = getinv)
}


## calculates inversion of matrix stored in 'makeCacheMatrix' object
## if object contains associatedValue returns it as inversion
## otherwise calculates inversion and additionally caches it in given object
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if ( !is.null(inv)) {
    return(inv)
  }
  matrix = x$get()
  inv <- solve(matrix)
  x$setinv(inv)
  
  inv
}

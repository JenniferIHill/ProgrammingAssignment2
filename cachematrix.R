## The following functions compute the inverse of a matrix object
## and store the inverse in a cache for quick retrieval in the
## future.

## The makeCacheMatrix function creates a matrix object that can
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of a matrix object
## created using the function makeCacheMatrix. If the inverse is
## already stored in the makeCacheMatrix function, then the cacheSolve
## function retrieves the stored inverse instead of recomputing it.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Matrix inversion is a costly computation and there may be some benefit to caching 
## the inverse of matrix, rather than compute it repeatedly. There are two functions
## to cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" which is really a list containing a
## a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse of the matrix
##4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## cacheSolve function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated, then the 
## function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

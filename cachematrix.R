## These functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix is a function that returns a list of functions and its
## purpose is to store a martix and a cached value of the inverse of the 
# matrix.

makeCacheMatrix <- function(x = numeric()) {
  cache<-NULL # holds the cached value or NULL if nothing is cached
  # store a matrix
  setMatrix <- function(newValue) { 
    x <<- newValue
    cache <<- NULL
  }
  getMatrix <- function() x  # returns the stored matrix
  cacheInverse <- function(solve) cache <<- solve # cache the given argument 
  getInverse <- function() cache # get the cached value
  # return a list
  list(setMatrix=setMatrix, getMatrix=getMatrix,
       cacheInverse = cacheInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix"
## returned by `makeCacheMatrix` above.

cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)){
     message("getting cached data")
     return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in the cache
  data <- y$getMatrix()
  inverse <- solve(data,...)
  y$cacheInverse(inverse)
  # return the inverse
  inverse
}

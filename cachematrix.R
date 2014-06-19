## Latest Changes - 2014.06.19 - 11:27 China Standard Time


## makeCacheMatrix creates a special matrix object with cacheable inverse.
## cacheSolve returns the inverse of a makeCacheMatrix object


### makeCacheMatrix takes as input a matrix object.  The output is a special
### matrix object that can cache its inverse.
### ------------------------------------------------------------------------
### <object>$get - returns the original matrix object.
### <object>$inverse - variable holding the cached inverse of the matrix.
### <object>$setinverse - stores the value of inverse in the cache.
### <object>$getinverse - returns the variable inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


### cacheSolve takes as input an object created by makeCacheMatrix and returns its
### inverse, calculated by the solve() function.  cacheSolve will attempt to look in
### the cache for the result, and failing that, will calculate the inverse and return
### that.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Retrieving inverse from cache.")
    return(inverse)
  }
  message("Calculating inverse not found in cache.")
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

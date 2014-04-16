## R file containing two functions:

## 1.makeCacheMatrix() for creation of a special cacheable matrix object, with
## functions allowing setting and retrieval of the original matrix and setting
## retrieving the inverse of the supplied matrix.
##
## 2.cachesolve() calculates the inverse of a supplied special matrix, and caches the values 
## for retrieval on subsequent calls of the function.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  ##create functions to set,get original matrix and set,get inverse of matrix 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ##functions to allow retrieval of previously cached inverse or setting the inverse 
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  ##create list of functions to attach to special matrix object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## 2.cachesolve() calculates the inverse of a supplied special matrix, and caches the values 
## for retrieval on subsequent calls of the special matrix objects function getinverse().

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## check for cached inverse of matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if no cached copy, use solve() to calulate inverse and set the value in special matrix object
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

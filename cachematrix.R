## makeCacheMatrix and cacheSolve work together to compute 
## and cache the inverse of a matrix. If the inverse has already
## been computed, the cached inverse will be retrieved.

## makeCacheMatrix will create a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve computes the inverse of the special "matrix"
## object returned by makeCacheMatrix. If the inverse has
## already been computed, then cacheSolve will retrieve the 
## inverse from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setinverse(inverseMatrix)
  inverseMatrix
}

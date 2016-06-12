## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y = matrix()) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse_matrix <<- inv
  getinverse <- function() inverse_matrix 
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setinverse(inverse_matrix)
  inverse_matrix
}

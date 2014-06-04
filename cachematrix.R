## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL
  set <- function(y) {
    x <<- y
    mInverse <<- NULL
  }
  get <- function() x
  set_inv <- function(p_mInverse) mInverse <<- p_mInverse
  get_inv <- function() mInverse
  
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(p_mMatrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  mInverse <- p_mMatrix$get_inv()
  if(!is.null(mInverse)) {
    message("getting cached data")
    return(mInverse)
  }
  
  data <- p_mMatrix$get()
  mInverse <- solve(data, ...)
  p_mMatrix$set_inv(mInverse)  
  mInverse
}

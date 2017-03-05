## This function will be used to find the inverse of a matrix using 
##caching mechanism to make use of caching in memory

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y = matrix()){
    x <<- y
    mInv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) mInv <- inverse
  getInverse <- function() mInv
  list(set = set, get = get, set=setInverse, get = getInverse)

}
## This function checks if the inverse of a matrixx is already in cache it returns
## the matrix with out doing any further computation.

cacheSolve <- function(x, ...) {
    
  mInv <- x$getInverse()
  ## return the inverse of the matrix if it is already in cache.
  if(!is.null(mInv)){
    
    return(mInv)
  }
  
  ## If the inverse of the matrix is not in cache it recalculates the inverse
  ## of the matrix.
  data <- x$get()
  
  mInv <- solve(data, ...)
  
  x$setInverse(mInv)
  
  return(mInv)
}

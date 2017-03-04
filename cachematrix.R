## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mInv <- x$getInverse()
  if(!is.null(mInv)){
    
    return(mInv)
  }
  
  data <- x$get()
  
  mInv <- solve(data, ...)
  
  x$setInverse(mInv)
  
  return(mInv)
}

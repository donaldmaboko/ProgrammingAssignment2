## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  Inv = NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() {x}
  setInv <- function(i) {Inv <- i}
  getInv <- function() {Inv}
  list(set=set, get=get, setInv=setInv, getInv=getInv)
  }


  
##Compute the inverse of 'makeCacheMatrix'. The 'cachesolve' retrieves the 
## inverse from the cache i.e. if the inverse is already calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  Inv = x$getInv()
  
  if(!is.null(Inv)) {
    message("getting cached inverse")
    return(Inv)
  }
  A <- x$get()
  Inv <- solve(A, ...)
  x$setInv(Inv)
  Inv
}
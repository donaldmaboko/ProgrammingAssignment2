## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix() ) {
  
  ## Initialize the inverse property
  Inv <- NULL
  
  ## Method to set the matrix
  set <- function(matrix) {
    x <<- matrix
    Inv <<- NULL
  }
  
  ## The matrix
  get <- function() {
    ## Returns the matrix
    x
  }
  
  ## Set the inverse of the matrix
  setInv <- function(inverse) {
    Inv <<- inverse
  }
  
  ## Get the inverse of the matrix
  getInv <- function() {
    ## Return the inverse property
    Inv
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}



## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x <- x$getInv()
  
  ## Just return the inverse if its already set
  if( !is.null(x) ) {
    message("getting cached data")
    return(x)
  }
  
  ## Get the matrix from our object
  A <- x$get()
  
  ## Use matrix multiplication to compute the inverse
  x <- solve(A) %*% A
  
  ## Set the inverse to the object
  x$setInv(x)
  
  ## Returns the matrix
  x
}








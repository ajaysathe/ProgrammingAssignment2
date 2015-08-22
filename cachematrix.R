## makeCacheMatrix produces a list of functions that get/set the matrix data, and get/set the matrix inverse. 
## cacheSolve computes the matrix inverse, either taking it from cache if it exists, or else computing it

## this function creates the four functions of get/set matrix data and get/set matrix inverse
makeCacheMatrix <- function(varX = matrix()) {
# ajaysathe
    # varX is a matrix
    # this function produces a list of functions that ...
    # set/get the value of a matrix
    # set/get the inverse of the matrix. 
  
  MatInv = NULL
  
  setMatrix = function(varY) 
  {
    varX <<- varY   # assign varY value to varX in a different environment
    MatInv <<- NULL
  }
  
  getMatrix = function() varX
  
  setInverse = function(inverse) MatInv <<- inverse 
  
  getInverse = function() MatInv
  
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


## This function computes matrix inverse, either by returning it from cache or by calculating it

cacheSolve <- function(x, ...) {
 #  x comes from the above function. This function should return the inverse of the original matrix

  invrs <- x$getInverse()
  
  if (!is.null(invrs)) # the inverse already exists. 
  {
    # take it from cache and return it 
    
    message("taking it from the cache")
    
    return(invrs)
  }
  
  # it does not exist in cache, so compute it now. 
  mat.data <- x$getMatrix()
  
  invrs <- solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setInverse(invrs)
  
  return(invrs)
}

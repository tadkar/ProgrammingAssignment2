## makeCacheMatrix is a function that contains the value of a matrix
## together with four functions that operate on that matrix
## cacheSolve finds and caches the inverse of the special matrix type 
## created by makeCacheMatrix

## The way makeCacheMatrix works is by using the value m as a flag
## The flag is NULL if the data is fresh 
## and contains the inverse if the data has not changed 

## This link is *very* helpful
## https://class.coursera.org/rprog-002/forum/thread?thread_id=696


makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y             ## This double arrow set <<- sets the value of x in the parent frame
    inv <<- NULL        ## This double arrow set <<- sets the value of inv in the parent frame
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse ##store the inverse and set the flag
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function first checks if the inverse has already been 
## found by checking the flag set in the special matrix type
## If the flag is not NULL, the inverse can be retreived
## Else the inverse is found and the flag is set to the inverse

cacheSolve function(x, ...) {
  ## Return the inverse of x
  i <- x$getInverse() 
  if(!is.null(i)) { ## check to see if the inverse has already been computed
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  matrixInv <- solve(data, ...)
  x$setInverse(matrixInv) ##store the inverese. This function will set the flag for us
  matrixInv
}

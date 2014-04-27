## makeCacheMatrix is a function that creates an object that 
## contains the value of a matrix, has space for its inverse
## together with four functions that operate on that matrix

## cacheSolve finds and caches the inverse of the special matrix type 
## created by makeCacheMatrix

## The way makeCacheMatrix works is by using the value m as a flag
## The flag is NULL if the data is fresh 
## Flag contains the inverse if the data has not changed 

## This link is *very* helpful
## https://class.coursera.org/rprog-002/forum/thread?thread_id=696

## get,set, getInverse and setInverse are helper functions that 
## live together with the data. 
makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL   ##Our flag
  
  ## Use lexical scoping to set the value of the matrix that comes with this type
  set <- function(y) {
    x <<- y             ## This double arrow set <<- sets the value of x in the makeCacheMatrix environment
    inv <<- NULL        ## This double arrow set <<- sets the value of inv in the parent frame
  }
  
  get <- function() x   ## Return the matrix stored within this special object
  
  setInverse <- function(inverse) inv <<- inverse ##Store the inverse and set the flag
  
  ##Return the value of the flag. If already computed this will be the inverse
  getInverse <- function() inv  
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function first checks if the inverse has already been found 
## by checking the flag set in the special matrix type
## If the flag is not NULL, the inverse can be retreived
## Else the inverse is found and the flag is set to the inverse

cacheSolve function(x, ...) {
  ## Return the inverse of x
  
  i <- x$getInverse() 
  
  if(!is.null(i)) { ## check to see if the inverse has already been computed
    message("getting cached data")
    return(i)
  }
  ## If we get here, the inverese has not been computed yet
  data <- x$get()
  matrixInv <- solve(data, ...) ## This is the R function for computing inverse
  x$setInverse(matrixInv) ##store the inverese. This function will set the flag for us
  
  matrixInv   ##Return the computed inverse
}

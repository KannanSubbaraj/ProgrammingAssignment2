## Put comments here that give an overall description of what your
## functions do

#Programming Assignment 2: Lexical Scoping

#Method 1: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#Method 2: cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Cache toggle bit initialization
  cacheBit <- NULL
    
  #set the value of the matrix
  SetMatrix <- function(y) {
    x <<- y
    # Toggling cache bit
    cacheBit <<- NULL
  }
  
  #get the value of the matrix
  GetMatrix <- function() {  x  }
  
  # Similarly Get Set methods for inverse 
  SetInverse <- function(Inverse) { cacheBit <<- Inverse }
  GetInverse <- function() { cacheBit }
  
  # return a list. Each named element of the list is a function
  list(SetMatrix = SetMatrix, GetMatrix = GetMatrix, SetInverse = SetInverse, GetInverse = GetInverse)
}


## Write a short comment describing this function
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inverse_Val <- x$GetInverse()
  if(!is.null(Inverse_Val)) {
    message("Getting cached value")
    return(Inverse_Val)
  }
  data <- x$GetMatrix()
  Inverse_Val <- solve(data)  ## Calculating the inverse
  x$SetInverse(Inverse_Val)
  Inverse_Val
}

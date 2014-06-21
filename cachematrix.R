## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## create a list with functions to get and set data and inverse of matrices

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## get the value of the matrix
  
  get <- function() x
  
  
  ## set the inverse of the matrix
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
 ## list to set and get inverse of a matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function gets the matrix to be inverted, apply Solve
## function and return the matrix. 

cacheSolve <- function(x, ...) 
{
  invmat <- x$getinverse()
  
  ##if inverse of the matrix is available return matrix
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  ## if not available: calculate inverse of the matrix and return   
  data <- x$get()
  invmat <- solve(data, ...)
  ## set the inverse of the matrix 
  x$setinverse(invmat)
  invmat
}

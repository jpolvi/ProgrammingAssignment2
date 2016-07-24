## Put comments here that give an overall description of what your
## functions do
# 
# functions for creating a cached matrix and 
# for solving its inverse matrix, structure adapted 
# from the vector example - Jussi Polvi 24.7.2016
#
## Write a short comment describing this function
#
# makecacheMatrix() creates a list of functions, containing
# functions to
#   1. set the value of the matrix 
#   2. get the value of the matrix 
#   3. set the inverse of the matrix 
#   4. set the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
#
# cacheSolve() uses function solve() to calculate
# the inverse of a cached matrix
#  - if a cached version the of solution exists,
#    the function uses it instead of recalculating

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
          ## Return a matrix that is the inverse of 'x'
}

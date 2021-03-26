## Set of two functions to cache an inverted matrix
## First function makeCacheMatrix to create an object to store a matrix (x) 
## Thanks to lexical scoping, this function stores a copy of the environment of 
## the function in the resulting matrix
## including the function defined within the function itself. 


makeCacheMatrix <- function(x = matrix()) {

  ## when called directly, it clears the cache and sets up functions for the environment 
  ## - for retrieving and setting the values

  inverted <- NULL
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  get <- function() x
  setinverted <- function(inverted) inverted <<- inverted
  getinverted <- function() inverted
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
  
}



## Second function cacheSolve to invert the matrix (if not done yet) or return the 
## cached inverted value.
## This function calls on the objects stored in the environment of the matrix (x) created by makeCacheMatrix
## First it retrieves the inverted value stored with x
## Then it tests whether it contains some value -> in which case it returns the cached value
## In case it is NULL, it calculates the inverted value, stores it in cache and returns the same

cacheSolve <- function(x, ...) {

  ## retrieve the cacacheched inverted matrix
  inverted <- x$getinverted()
  
  ## test if the inverted matrix exists and is not NULL .. in such case return the cached value
  if(!is.null(inverted)) {
    message("getting cached inverted matrix")
    return(inverted)
  }
  
  ## in case it has not been cached yet, it retrieves the matrix, inverts it and stores the inverted value in cache
  data <- x$get()
  inverted <- solve(data, ...)
  x$setinverted(inverted)
  
  ## Return a matrix that is the inverse of 'x'
  inverted
  
  }

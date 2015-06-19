## makeCacheMatrix will create a cached version of a matrix
makeCacheMatrix <- function(x = matrix()) {
  ## Create a variable, i, to cache the inverse of the matrix
  i <- NULL
  
  ## Create the set, get, setInverse, and getInverse functions
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() { x }
  setInverse <- function(inverse) { i <<- inverse }
  getInverse <- function() { i }
  
  ## Return a list of the functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve will return the inverse of a matrix, or a the cached version of the inverse of the matrix
cacheSolve <- function(x,...) {
  
  ## Check if the inverse of the matrix has already been calculated, and if so return the cached value		
  i <- x$getInverse()
  if (!is.null(i)) {
    message("Returning cached inverse.")
    return(i)
  }
  
  ## Solve for the inverese of the matrix and cache the result
  i <- solve(x$get(),...)
  x$setInverse(i)
  
  ## Return the inverse of the matrix
  i
}




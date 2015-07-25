## As calculating the Inverse can be an expensive operation the two following
## functions can be used to cache the result, so does not need to be repeatedly
## calcuated for the same Matrix.

## The makeCacheMatrix function creates an object that can be used to cache
## the inverse Matrix.

makeCacheMatrix <- function(x = matrix()) {
  # Cache the Matrix and clear any cached Inverse Matrix
  set <- function(x) {
    a <<- x
    b <<- NULL
  }
  
  # Return the Cached Matrix
  get <- function(x) {
    a
  }
  
  # Cached the Inverse Matrix
  setInverse <- function(x) {
    b <<- x
  }
  
  # Return the Cached Inverse Matrix
  getInverse <- function() {
    b
  }
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve fuction uses the object created by makeCacheMatrix function
## to calculate and cache the result of solve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Get the cached Matrix Inverse
  reciprocal <- x$getInverse()
  
  # If it has been calculated return it
  if(!is.null(reciprocal)) {
    message ("Return cached Inverse Matrix")
    return(reciprocal)
  }
  
  # Get the cached Matrix
  a <- x$get()
  
  # Find the Inverse
  reciprocal <- solve(a)
  
  # Cache the Inverse and return the value
  x$setInverse(reciprocal)
  reciprocal
}

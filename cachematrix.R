## What my functions do (please see explanation 1. & 2. below)

## 1. 
## The "makeCacheMatrix" function below works to create a matrix object that
## can cache it's inverse.
## First, the "inv" object is defined as 'Null' to be the eventual storage
## site of the inverted matrix once cached.
## As per the "Caching the mean of a vector" example for this assignment,
## functions to 'set' and 'get' the matrix "x" as well as
## functions to 'set' and 'get' the inverse of the matrix were created using
## the "<<-" operator to be applicable across environments.
## Next, the 'set' function was used to define the matrix "x" & clear
## previous cached inverses.
## Next, the 'get' function was used to get the current matrix "x".
## Then two functions were defined to set the inverse as well as get the
## cached inverse for the "inv" object defined first.
## Finally, a list was created to allow the return of the above functions.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 2. 
## The "cacheSolve" function below works to 
## "Return a matrix that is the inverse of 'x'" as per the instructions.
## First, to get the cached inverse, the "inv" object is defined.
## Next, an 'if' function is used to write a message to be returned if the
## cached inverse exists.
## If the cache inverse does not exist, the "data" object is defined to
## access matrix data.
## This matrix data is then inverted via the newly defined "inv" object
## through the solve function.
## This new inverse is then 'set' into the cache.
## Finally, the calculated inverse is returned.


cacheSolve <- function(x, ...) {
      
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}


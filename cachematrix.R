## Functions below calculate inverse matrix and cache calculated results

## Function makeCacheMatrix creates a list of functions for calculating and caching inverse matrix
## Returned functions:
## set(y) - sets matrix for which to calculate inverse and invalidates cached inverse matrix data
## get() - gets matrix data to be inversed
## setinvmat(invmat) - sets values for inversed matrix
## getinvmat() - returns inversed matrix data

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize datra variable for inversed matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmat <- function(invmat) m <<- invmat
  getinvmat <- function() m
  ## Returns a list of functions
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


## Function cacheSolve returns inversed matrix data. Gets data from cache if it exists.
## Parameters:
##  x - CacheMatrix list of functions created by function makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Gets cached inversed matrix data
  m <- x$getinvmat()
  ## If there is cached inversed matrix data return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If there is no cached inversed matrix data calculate it
  ## Get matrix to be inversed
  data <- x$get()
  ## Inverse matrix
  m <- solve(data)
  ## Set inversed matrix data
  x$setinvmat(m)
  ## Return inversed matrix data
  m
}
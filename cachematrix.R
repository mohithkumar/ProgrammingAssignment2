## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set(), get(), setsolve(), getsolve() are all internal functions
  set <- function(y) {
    ## 'x' and 'm' are cached variables
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## 'm' is the internal variable here.
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## If the internal variable 'm' is null, then its inverse is calculated
  m <- solve(data, ...)
  
  x$setsolve(m)
  ## Return m
  m
}

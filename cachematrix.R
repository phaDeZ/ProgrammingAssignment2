## Creation and calculation of cacheable matrices and its inverse.

## Returns a list representing a cacheable matrix object
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
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


## Looks up if the matrix was already solved and returns the inverse.
## If the inverse was not yet calculated the calculation will be done before.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  ## Return a matrix that is the inverse of 'x'#  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

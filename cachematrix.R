## This function caches a matrix and calls the solve function
## to create the inverse of the matrix and cashes that.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Returns the inverse of a matrix
## Checks to see if it has already been cached
## If so, it returns the cached inverse matrix
## Otherwise it calculates the inverse and returns it.
cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("Getting Cached Data")
    return(m)
  }
  
  matdata <- x$get()
  m <- solve(matdata, ...)
  x$setinv(m)
  m
  
}

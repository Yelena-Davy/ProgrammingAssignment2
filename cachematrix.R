## Functions cache potentially time consuming calculations for inverse of special matrix.
## The values of matrix and it inverse are cached so if matrix is not changed it inverse 
## can be looked up rather then recalculated.
## Caching is based on the scoping rules of R language for preservig state inside of R object.


## makeCacheMatrix creates a special  list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
      setsolve  = setsolve ,
      getsolve  = getsolve )
}

## The following function calculates the inverse of the special "matrix" created with 
## the function "makeCacheMatrix". 
## It first checks to see if the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
        ## Return a matrix that is the inverse of 'x'
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

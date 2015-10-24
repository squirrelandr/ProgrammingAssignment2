## This program would dynamically store a matrix value in the global environment
## When called, it would check if the matrix value exists in the global environment
## If existing, it would return the stored value
## If non-existing, it would calculate the inversed matrix using solve() function

## this function returns a list of function for the cacheSolve function to use

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() { x }
  setinv <- function(inv) { inv <<- inverse }
  getinv <- function() { i }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## this function checks the existence of the stored global value
## if existing, return the stored value
## if non-existing, inverse the matrix and return the inversed matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

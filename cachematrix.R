## This function is to store a temporary inversed matrix in the environment
## 

## This function creates a special "matrix" object that can cache its inverse
## Not sure if these would work

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  m <- null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
    data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## INPUT: invertible matrix
## examples: matrix(1:4, 2,2);  matrix(rnorm(16),4,4)
##
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## example of INPUT: makeCacheMatrix(matrix(1:4,2,2))
## OUTPUT: inverse 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(solve) m <<- solve
  
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## Put comments here that give an overall description of what your
## These functions solve for the inverse of a matrix by caching the matrix inversion

## Cache matrix inversion
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


## call inverse matrix from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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



# Test Data
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)

x=makeCacheMatrix(A)

cacheSolve(x)

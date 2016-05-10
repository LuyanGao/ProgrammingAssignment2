##  Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
      x <<- y
      n <<- NULL
}

  get <- function() x
  setinv <- function(inverse) n <<- inverse
  getinv <- function() n
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The function below computes the inverse of the Matrix created by makeCacheMatrix. 
## If the inverse has already been calculated, then it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  n <- x$getinv()
  if(!is.null(n)) {
      message("getting cached inverse matrix")
      return(n)
   } 
   data <- x$get()
   n <- solve(data, ...)
   x$setinv(n)
   n
}

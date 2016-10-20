makeCacheMatrix <- function( x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinve <- function(inve) inv <<- inve
  getinve <- function() inv
  list(set = set, get = get,
       setinve = setinve, getinve = getinve)
}

cacheSolve <- function(x, ...) {
  i <- x$getinve()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinve(i)
  i
}
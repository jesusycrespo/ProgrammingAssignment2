makeCacheMatrix<- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list( get = get,
        setinv = setinv,
        getinv = getinv)
}

## depuro la matriz

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

makeMatrix <- function(m = matrix()) {
    i <- NULL
  set <- function(n) {
          m <<- n
          i <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(t, ...) {
  i <- t$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- t$get()
  i <- solve(data, ...)
  t$setinverse(i)
  i
}
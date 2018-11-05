makeCacheMatrix <- function(x = matrix()) {
  inversemat <- NULL
  set <- function(y) {
    x <<- y
    inversemat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversemat <<- inverse
  getinverse <- function() inversemat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inversemat <- x$getinverse()
  if(!is.null(inversemat)) {
    message("getting cached data")
    return(inversemat)
  }
  finaldata <- x$get()
  inversemat <- solve(finaldata, ...)
  x$setinverse(inversemat)
  inversemat
}

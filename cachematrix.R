## These functions set up a matrix in a way that the second function can retrieve
## the inverse or calculate it.

## This functions sets up the matrix to be ready to be run through the cacheSolve
## function. A matrix that has not been set up this way, will not return a result
## when the cacheSolve function is called.



makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) j <<- inverse
  getinverse <- function() j
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This functions first tries to retrieve a previously calculated inverse. If
## the inverse has not been calculated, it will calculate it. Otherwise, it will
## return the previously calculate inverse, thus saving R from having to calculate.

cacheSolve <- function(x, ...) {
  j <- x$getinverse()
  if(!is.null(j)) {
    message("getting cached data")
    return(j)
  }
  data <- x$get()
  j <- solve(data, ...)
  x$setinverse(j)
  j
}
## a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  x # i don't understand why i need this.  without it, $get() returns the special "matrix" object and not the matrix x
  i <- NULL
  ## This function changes the matrix after creation and invalidates the cached inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## This function gets the matrix
  get <- function() x
  ## this function caches the inverse
  setinverse <- function(inverse) i <<- inverse
  ## this function gets the cached inverse (if cached)
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated by
## cacheSolve (and the matrix has not changed), then cacheSolve retrieves the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  # save the cached inverse (if cached) to the local environment
  i <- x$getinverse()
  # if the inverse is not cached, solve for the inverse, save it to the local
  # environment, and cache it for later
  if (is.null(i)) x$setinverse(i <- solve(x$get(), ...))
  # return the inverse saved in the local environment
  i
}

## test method
test <- function() {
  x <- rbind(c(1, -1/4), c(-1/4, 1))
  print(x %*% solve(x))
  x <- makeCacheMatrix(x)
  print(x$getinverse())
  print(x$get() %*% cacheSolve(x))
  print(x$get() %*% x$getinverse())
}
#test()
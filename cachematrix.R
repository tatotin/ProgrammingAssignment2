# The function sets the inverse of a matrix 
# Then, once is on cache there is no need to calculate again (it is called from cache)
# As a function that store in cache, has a set() and a get() method

makeCacheMatrix <- function( m = matrix() ) {
  i <- NULL
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  get <- function() {
    m
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {
    i
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function returns a matrix that is the inverse of 'x'
## Before calculate, it check if it is already set (then get the cached data).
##Finally, return the inverse matrix that is the inverse of the given x

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
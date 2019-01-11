## Put comments here that give an overall description of what your
## functions do

## Creates a cached matrix object

makeCacheMatrix <- function(x = matrix ()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    y <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- solve(x)
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the cached matrix from makeCacheMatrix function. 
## If the inverse was already calculated, it retrieves it from the cache, saving time 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("retrieving cached data")
    return(inv)
  }
  inv <- solve(x$get())
  x$setInverse(inv)
  inv
}

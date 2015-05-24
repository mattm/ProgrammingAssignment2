## Demonstrate R scoping rules to preserve state

# Given a matrix, this function returns a list of functions that can
# be used to change the value of the matrix (set), obtain the value
# of the matrix (get), set its cached inverse (setCachedInverse), or to
# obtain the cached inverse (getCachedInverse)
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setCachedInverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  getCachedInverse <- function() cachedInverse
  list(set = set, get = get, setCachedInverse = setCachedInverse, getCachedInverse = getCachedInverse)
}

# Given a cached matrix created with the function above, this function
# returns its inverse either by getting the cached value or by calculating
# it (and then saving it in the cache so it doesn't have to be calculated again)
cacheSolve <- function(x, ...) {
  cachedInverse <- x$getCachedInverse()
  if (!is.null(cachedInverse)) {
    return(cachedInverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setCachedInverse(inverse)
  inverse
}

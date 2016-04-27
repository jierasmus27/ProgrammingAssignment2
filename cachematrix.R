## Assignment: Caching the Inverse of a Matrix

## The first function creates a matrix cache variable that stores previously seen matrices

makeCacheMatrix <- function(x = matrix()) {
  matrix_cache <- NULL
  
  set <- function(y) {
    x <<- y
    matrix_cache <<- NULL
  }
  get <- function() x
  
  setMatrixCache <- function(matrix) matrix_cache <<- matrix
  getMatrixCache <- function() matrix_cache
  
  list(set = set, get = get, setMatrixCache = setMatrixCache, getMatrixCache = getMatrixCache)
}

## Retrieve the inverse matrix from the matrix cache, if possible - or add it to the cache

cacheSolve <- function(x, ...) {
  matrix_cache <- x$getMatrixCache()

  if (!is.null(matrix_cache)) {
    message("getting cached data")
    return(matrix_cache)
  }
  
  matrix <- x$get()
  matrix_cache <- solve(matrix, ...)
  x$setMatrixCache(matrix_cache)
  matrix_cache
}

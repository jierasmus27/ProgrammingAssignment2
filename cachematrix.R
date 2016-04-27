## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL

  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x

  setimatrix <- function(matrix) cache <<- matrix
  getimatrix <- function() cache

  list(set = set, get = get, setimatrix = setimatrix, getimatrix = getimatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  cache <- x$getimatrix()

  if (!is.null(cache)) {
    print("Getting chached data")
    return(cache)
  }

  matrix <- x$get()
  cache <- solve(matrix, ...)
  x$setimatrix
  cache
}

cacheSolve()

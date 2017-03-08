## use makeCacheMatrix to create a cached matrix that will store the result of the inverse in a cache,
## use cacheSolve to return the inverse of the matrix

## Put the matrix in a list with its methods to set and the inverse of the matrix to cache it.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Verify in the cached matrix if the inverse has already been calculated. 
## If so, return the inverse of the matrix in memory. Calculate it and store it otherwise.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inversematrix(data, ...)
  x$setinv(m)
  m
}

## Calculate the inverse of a matrix with the solve function
inversematrix <- function(data, ...) {
  solve(data,...)
}

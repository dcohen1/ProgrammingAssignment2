## These functions work together to solve and cache the inverse of a matrix
## makeCacheMatrix is a wrapper for cacheSolve that create a list that
## used to store information on whether the inverse matrix function
## has been carried out for a given dataset


makeCacheMatrix <- function(x = matrix()) {
  ## initialize m as NULL
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## sets up get to retrieve the data in the matrix
  get <- function() x
  ## setinv can be passed a value of the inverse matrix or assumes a NULL state
  setinv <- function(inv) m <<- inv
  ## getinv will adopt the value inverse matrix calculated in CacheSolve
  ## or a NULL state 
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## CacheSolve returns either the cached value of the inverse matrix or calculates it anew
## This function only works on square, invertible matrices

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## checks for square matrix
  if (nrow(data) == ncol(data)){
    m <- solve(data, ...)
    ## passes the value of the inverse matrix to makeCacheMatrix function
    x$setinv(m)
    m}
  else {print("Matrix is not square")}
}
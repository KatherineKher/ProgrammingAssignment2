## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Defining makeCache function and passing Matrix to it


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y                                     ##sets the new value
    m <<- NULL                                  ##clear cached value
  }
  get <- function() x                           ##gets matrix value
  setinv <- function(matrix_) m <<- matrix_     ##evaluates matrix inverse
  getinv <- function() m                        ##get matrix inverse
  list(set = set, get = get,                    ##give names to defined functions
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## Defining cacheSolve function and passing Matrix to it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setinv(m)
  m
}

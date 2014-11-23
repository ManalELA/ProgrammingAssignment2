
## In this function, we will test first if the matrix is square or not,
## and then create the special matrix

makeCacheMatrix <- function(x = matrix()) {
  d<-dim(x)
  if(d[1]==d[2]){
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }
  else{
    message("This matrix is not square!")
  }
}


## this function will return the inverse of the matrix based on the result of the first function

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

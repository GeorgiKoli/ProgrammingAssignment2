# provides caching capabilities for matrix inversion,
# which is computationally expensive operation

## creates a 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # i will store the inverse matrix
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inv) {
    i <<- inv
  }
  
  # will return the cached inverse matrix to cacheSolve()
  getinverse <- function() {
    i
  }
  
  # returns a list object to the caller
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## computes the inverse of a matrix
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  
  i <- solve(data)
  
  x$setinverse(i)
  
  i
}

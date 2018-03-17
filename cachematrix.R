##Computing the inverse of a square matrix

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set1 <- function(y) {
    x <<- y
    i <<- NULL
  }
  get1 <- function() x
  setinverse1 <- function(inv) {
    i <<- inv
  }
  getinverse1 <- function() i
  
  list(set1 = set1, get1 = get1, setinverse1 = setinverse1,
       getinverse1 = getinverse1)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse1()
  if(!is.null(i)){
    message("Getting cached data")
    return(i)
  }
  data <- x$get1()
  i <- solve(data) %*% data
  x$setinverse1(i)
  i
}

##These functions take a matrix as input
## and output is inverse of the matrix
## Require that the matrix is square matrix, no check is made

## Function to set/ get value of matrix and set/get 
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) i <<- Inv
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Function to invert the matrix but first
## check to see if the inverted matrix is already
## stored in cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInv()
  ## check if cache exists
  if(!is.null(i)) {  
  message("getting cached data")
  return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}
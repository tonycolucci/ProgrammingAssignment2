## These functions combine to allow a user to solve for the inverse of a matrix
## and cache the result, making for faster retrieval if they need to solve the same matrix

## This function creates a list of functions that we will reference within cachesolve and
## associates them with the specific matrix passed in, providing the reference so cachesolve
## knows when it can use the cached value or if it needs to recalculate the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function calculates the inverse of a matrix associated with the list passed in from
## the argument x or returns the previously calculated value if that had been cached more 
## recently than we have called makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if(!is.null(I)){
    message('getting cached data')
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}

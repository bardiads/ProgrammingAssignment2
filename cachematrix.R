## This function make a new matrix object which keeps track of its
## inverse through getInverse, and setInverse methods

## this function return a list of following functions:
##  set, and get which assign or retrieve a new matrx 
## setInverse, and getInverse which assign or retrieve
## the reversed version of matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse.x <- NULL
    set <- function(y) {
      x <<- y
      inverse.x <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverse.x <<- inverse
    getInverse <- function() inverse.x
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve get this matrix object build with previous methods
## and calculate its inversed if it is not already calculated before
## it sets the calculated inversed matrix in input object x, and return

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse.x <- x$getInverse()
    if(!is.null(inverse.x)) {
      message("getting cached inverded matrix")
      return(inverse.x)
    }
    data.matrix <- x$get()
    inverse.x <- solve(data.matrix, ...)
    x$setInverse(inverse.x)
    inverse.x  
}

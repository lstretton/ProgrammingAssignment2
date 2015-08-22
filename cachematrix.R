## file contains 2 functions.
## 1. makeCacheMatrix(): returns a list containing functions to:
##    a. set the value of a matrix
##    b. get the value of a matrix
##    c. set the inverse of the matrix
##    d. get the inverse of the matrix
## 2. cacheSolve(): returns the inverse of a matrix by:
##    a. calculating using the solve function if inverse hasnt already been calculated; OR
##    b. from the m object if the inverse has already been calculated

## creates a list containing functions to manage a matrix
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverse <- function(inversematrix) m <<- inversematrix
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Calculates the inverse of the matrix 'x'
cacheSolve <- function(x) {
   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   m <- solve(x$get())
   x$setinverse(m)
   m
}


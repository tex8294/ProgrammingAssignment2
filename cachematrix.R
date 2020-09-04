## Put comments here that give an overall description of what your
## functions do

## This function create a special matrix that is able to cache its
## inverse. The function takes as an argument a matrix and gives a 
## list containing four function:
## 1) set: stores the matrix 
## 2) get: gives the value of the matrix stored
## 3) setinverse:  stores the matrix inverse
## 4) getinverse: gives the stored matrix inverse value

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
             x <<- y
             inv <<- NULL
       }
       get <- function() x
       setinverse <- function(inver) inv <<- inver
       getinverse <- function() inv
       list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}



## This function takes as an argument the output of the previous
## function, computes the matrix inverse and stores it.
## In case the matrix inverse has been previously stored, the function
## notifies the user and retrieves the stored value.

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
       if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
       }
       data <- x$get()
       inv <- solve(data, ...)
       x$setinverse(inv)
       inv
}


## The CacheMatrix file contains two functions makeCacheMatrix and cacheSolve
## These functions help create a special matrix that can cache the inverse of a 
## matrix, so as to reduce Matrix inversion which is usually a costly computation


## The makeCacheMatrix function creates a special "matrix", 
##   which is really a list containing a function to:
##        set the value of the matrix
##        get the value of the matrix
##        set the value of the inverse matrix
##        get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     ## 'x' is the parameter passed to the function.
     ## the assumption is that x will be a square invertible matrix. 
     
     inv <- NULL
     
     ## Sets the data of the Normal Matrix
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     ## Returns the data of the Normal Matrix  
     get <- function() x
     
     ## Stores the Inverse Matrix
     setinverse <- function(invMatrix) inv <<- invMatrix
     
     ## Returns the Inverse Matrix
     getinverse <- function() inv
     
     ## creates and returns the list object which acts as a CacheMatrix
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" 
##   returned by makeCacheMatrix above. 
## If the inverse has already been calculated, 
##   then cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     
     ## Return a matrix that is the inverse of 'x'
     inverseM <- x$getinverse()
     
     ## Check if the Inverse is cached. If cached, return the cached data
     if(!is.null(inverseM)) {
          message("getting cached data")
          return(inverseM)
     }
     
     ## If the data is not cached. The below statements will execute.
     ## In the below code, we get the matrix data, compute the inverse
     ## and store the inverse, for future use
     data <- x$get()
     inverseM <- solve(data, ...)
     x$setinverse(inverseM)
     
     ## Return the inverse
     inverseM
}

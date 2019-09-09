## makeCacheMatrix creates a matrix which consists of 4 functions, 
#which, in that order, set the value of a vector, get that value, set 
#the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) inv <<- solve
   getinverse <- function() inv
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function checks if there already is a cached value for the
#inverse matrix. If yes, it skips the computation. If not,
#it computes the inverse matrix and stores it in the cache. 

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


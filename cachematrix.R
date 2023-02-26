## makeCacheMatrix takes a given invertible matrix and 
## packages that matrix into a list that serves as a wrapper class
## 
## cacheSolve will then take the output of makeCacheMatrix and calculate
## the inverse.

## makeCacheMatrix takes a matrix and packages it into a list. This
## list is a sort of wrapper class where the methods are set, get
## setinverse, and getinverse. setinverse caches, or stores, the inverse,
## while getinverse retrieves the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
 set <- function(y){
   x <<- y
   i <<- NULL
 }
 get <- function() x
 setinverse <- function(inverse) i <<- inverse
 getinverse <- function() i
 list(set = set, get = get, 
      setinverse = setinverse,
      getinverse = getinverse)
}


## cacheSolve takes the wrapper class object produced by makeCacheMatrix 
## and calls methods from the wrapper object. First it calls
## getinverse() to check if the inverse is already calculated and computed.
## If not, it uses solve() to calculate the inverse, then calls
## setinverse() to cache the inverse inside the wrapper object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## check if inverse is already calculated
  #Check if inverse is already calculated 
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

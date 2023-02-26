## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 #Initialize the inverse matrix to be empty
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


## Write a short comment describing this function

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

## To compute the inverse of a matrix and store in cache for future.
## If inverse is available in cache in that case no computation occurs
## inverse is retrieved from cache and displayed

## Creates a specail matrix object with get and set methods to get and 
## set the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse.matrix <- NULL
  set <- function(y){
    x <<- y
    inverse.matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse.matrix <<- inverse
  getinverse <- function() inverse.matrix
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  }


## Function to look for the cache for inverse
## return if avaialbe else computes a inverse and store in cache return inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse.matrix <- x$getinverse()
    if(!is.null(inverse.matrix)){
      message("getting cached data")
      return(inverse.matrix)
    }
    data <- x$get()
    inverse.matrix <- solve(data, ...)
    x$setinverse(inverse.matrix)
    inverse.matrix
}

## Test Case
# m = matrix(c(4,2,7,6), nrow=2, ncol=2)
# t <- makeCacheMatrix(m)
# cacheSolve(t) - Display inverse 
# cacheSolve(t) - Display inverse from cached data
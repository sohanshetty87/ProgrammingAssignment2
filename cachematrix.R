## MakeCacheMatrix() has 4 functions inside it
## setMatrix caches the matrix by saving it 
## getMatrix retruns the Cached Matrix
## setInverse caches the Inverse Matrix
## getInverse returns the Inverse Matrix 
## MakeCacheMatrix() returns a list of function names

## Write a short comment descr
## ibing this function

makeCacheMatrix <- function(x = matrix()) {
  temp <- NULL
  setMatrix <- function(y){
    x <<- y
    temp <<- NULL
  }
  
  getMatrix <- function() x
  
  setInverse <- function(inverseMatrix) temp <<- inverseMatrix
  
  getInverse <- function() temp
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse
  )
  
}


## cacheSolve() accepts an argument and checks if the inverse has been cached
## If the inverse has been cached, it returns the cached value without recomputing
## If the inverse hasn't been cached, it computes the inverse ,caches it and then returns it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  temp <- x$getInverse()
  if(!is.null(temp)){
    message("Getting Cached Data")
    return(temp)
  }
  temp <- x$getMatrix()
  temp <- solve(temp)
  x$setInverse(temp)
  temp
}

## This function creates a special "matrix" object that can cache its inverse.
## This matrix function will create a special matrix object and have  getter & setter functions  
makeCacheMatrix <- function(x = matrix()){
  
  i <- NULL  # Invese if assigned as null
  
  # Sets the matrix
  set <- function(y){
    x <<- y
    i <- NULL
  }
  
  # Gets the matrix
  get <- function(){
    x  # Return the matrix
  }
  
  # Sets the inverse Matrix
  setInverse <- function(inverseMatrix){
    i <<- inverseMatrix
  }
  
  # Gets the inverse Matrix
  getInverse <- function(){
    i
  }
  
  list(set=set, get=get, setInverse=setInverse,getInverse=getInverse)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## This function will calculate the invese of matrix . It returns the inverse from cache if available else find inverse & return  
cacheSolve <- function ( x,...){

  i <- x$getInverse()
  
  if (!is.null(i)){
    message("Getting the matrix from Cache")
    print(i)
  }
  
  dataMatrix <- x$get()
  
  i <- solve(dataMatrix)
  
  x$setInverse(i)
  
}
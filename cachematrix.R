########################################################################################
## The main purpose of the following two functions is to cache the inverse of a matrix##
########################################################################################
 
## The following function creates a special "matrix" object that can cache its inverse.
## Include the following functions:
## 1)set the matrix
## 2)get the matrix
## 3)set the inverse of a matrix
## 4)get the inverse of a matrix
## 5)list all functions
makeCacheMatrix <- function(x = matrix()) {
  ## initializing the inverse of a matrix
  init_inverse <- NULL
  
  ## set the matrix value
  set <- function(y) {
    x <<- y
    init_inverse <<- NULL
  }
  
  ## get the matrix value 
  get <- function() x
    
  ## set the inverse of a matrix
  setinverse <- function(input_inverse) init_inverse <<- input_inverse
  ## get the inverse of a matrix
  getinverse <- function() init_inverse
  
  ## return a list of methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the inverse of the special "matrix" 
## obtained by above makeCacheMatrix function. If the inverse of 
## the matrix has already been created, then cacheSolve function 
## should obtain the inverse of the matrix from the cache.
## Return a matrix that is the inverse of 'x'  
cacheSolve <- function(x=matrix(), ...) {

  ## get the inverse of matrix 
  init_inverse <- x$getinverse()
  
  # check if the inverse of matrix exists,
  # if exists, obtain cached the inverse of matrix 
  if(!is.null(init_inverse)) {
    message("getting cached the inverse of matrix")
    return(init_inverse)
  }
  
  # otherwise, obtain the matrix first
  data <- x$get()
  
  # and then, calculate the inverse of matrix using solve()
  init_inverse <- solve(data, ...)
  
  # cache the inverse of the matrix
  x$setinverse(init_inverse)
  
  # return the final matrix that is the inverse of 'x'
  init_inverse		
		
}

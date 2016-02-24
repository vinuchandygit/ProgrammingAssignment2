## Caching the Inverse of a Matrix
## Matrix inversion is a computation to caching the inverse of a matrix 
## rather than computing it repeatedly 
## Assignment is to write a pair of functions that cache the inverse of matrix

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cval <- NULL
    
  set <- function(y){
    x <<- y
    cval <<- NULL
  }
		  
  get <- function() x
	     
  setsolve <- function(solve) cval <<- solve
  
  getsolve <- function() cval

  list(set=set,
       get=get,
       setsolve=setsolve,
       getsolve=getsolve)				  
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cval <- x$getsolve()
    
  if(!is.null(cval)){
    message("gettingchached data")
    return (cval)
  }
		  
   data <- x$get()
   cval <- solve(data)
	       
   x$setsolve(cval)
   cval
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL                           
  set <- function(k) {                    
    x <<- k                            
    inv <<- NULL                        
  }
  get <- function() x                     
  
  sinverse <- function(inverse) inv <<- inverse  
  ginverse <- function() inv                    
  list(set = set, get = get, sinverse = sinverse, ginverse = ginverse)
}


## Write a short comment describing this function
##This function computes the inverse of the "matrix" created by makeCacheMatrix above. If the inverse has already been calculated and the matrix has not changed, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$ginverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$sinverse(inv)
  inv
}

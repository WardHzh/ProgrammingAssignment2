## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function returns a list of functions to set/get the value of matrix/inverse

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(z) {
     x <<- z
     inv <<- NULL
   }
   
   get <- function() x
   setinv <- function(inv1) inv <<- inv1
   getinv <- function() inv
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix and store in the cache. If the inverse has been 
## computed already, the function will just refer to the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  matrixInverse <- x$get()
  inv <- solve(matrixInverse,...)
  x$setinv(inv)
  inv
}

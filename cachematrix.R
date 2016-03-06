## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that it and its  can be cached.

makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## Computes the inverse of the matrix. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse that exists in the cache.
## C is  a variable used to ensure access to makeCacheMatrix  elements for calculation
cacheSolve <- function(x, ...) {
  C<-makeCacheMatrix(x)
  i  <- C$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- C$get()
  i  <- solve(data, ...)
  C$setinverse(i)
  i
}

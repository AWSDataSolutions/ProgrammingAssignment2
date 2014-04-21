##Matrix inversion is usually a costly computation and their may be some 
##benefit to caching the inverse of a matrix rather than compute it repeatedly. 
##This assignment consists of a pair of functions that cache the inverse of 
##a matrix.

## I Tested the following
##      [,1] [,2]
##[1,]   4    7
##[2,]   2    6

## Resulting Inverse, which is correct.  
##       [,1] [,2]
##[1,]   0.6  -0.2
##[2,]   -0.7 0.4

## Tried the following 4X4 and cached version was very slighly faster
##       [,1] [,2] [,3] [,4]
##[1,]     2    3    1   5 
##[2,]     1    0    3   1   
##[3,]     0    2   -3   2 
##[4,]     0    2    3   1

##makeCacheMatrix: This function creates a special "matrix" object that 
##can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  ##set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##get the value of the matrix
  get <- function() x
  
  ##set and get the value of the inverse
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve (data, ...)
  x$setinverse(m)
  m    
  
}

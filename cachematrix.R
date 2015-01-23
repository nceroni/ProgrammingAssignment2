## These function are a pair of functions that cache the inverse of a matrix. 
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # m is the inverse that is going to be cached
  m <- NULL
  # set function is for setting the matrix in cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get function is for get the cache matrix
  get <- function() x
  
  # this function calculate the inverse of the matrix and store in cache
  setinverse <- function(inverse) m <<- inverse
  
  # this function get the inverse of the matrix setied in the cache
  getinverse <- function() m
  
  # the list of the functions 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # This get the return of the inverse of the matrix
    m <- x$getinverse()
    # this if return the inverse, if its exist (distinct of NULL) and then finish the function
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    
    # If dont exist the inverse function, this get the matrix stored in the cache
    data <- x$get()
    
    # this calculate the inverse of the matrix
    m <- solve(data)
    
    # this store the inverse of the matrix in the cache for next execution
    x$setinverse(m)
    
    #return the inverse of the matrix
    m
}

## The following R functions will create an object from a matrix that can cache
## it's inverse, and a function to compute the inverse of a matrix and 
## save it to an object, or retrieve the inverse if it has already been computed.

## To test this, one can run the following:
## Make a matrix that can cache a value (but does not have one yet)
# x <- makeCacheMatrix(matrix(c(7,14,21,12,84,6),nrow=5,ncol=5))
## Compute and cache the inverse
# cacheSolve(x)
## this will return the inverse. Now, it is also saved. Re-run
# cacheSolve(x)
## you will get the output "getting cached data" before it returns 
## the inverse (no re-computing). 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # initialize m as nothing
  # define internal functions
  set <- function(y) { # function to set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x   # function to return the matrix
  setinv <- function(inverse) m <<- inverse   # will cache the inverse to whatever is supplied
  getinv <- function() m   # will return the inverse, or NULL if it is not calculated 
  list(set = set, get = get,   # store the functions in a list
       setinv = setinv,
       getinv = getinv)
}


##  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated (and 
##  the matrix has not changed), then the cacheSolve should retrieve the 
##  inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()   # call internal function  to get the inverse on makeCacheMatrix object. 
  if(!is.null(m)) {   # If there is a value initialized for inverse    
    message("getting cached data")  #print message 
    return(m) # return value of the inverse and exit
  }
  data <- x$get() #get the matrix values from the object
  # compute the inverse
  m <- solve(data, ...) #compute the inverse
  x$setinv(m)   # then set the inverse to previously computed value 
  m   ## Return a matrix that is the inverse of 'x'
}

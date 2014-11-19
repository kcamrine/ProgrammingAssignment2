## The following R functions will create an object from a matrix that can cache
## it's inverse, and a function to compute the inverse of a matrix and 
## save it to an object, or retrieve the inverse if it has already been computed.

## To test this, one can run the following:
## Make a matrix that can cache a value
# x <- makeCacheMatrix(matrix(c(7,14,21,12,84,6),nrow=5,ncol=5))
## Compute the inverse if it has not already done so
# cacheSolve(x)
## this will return the inverse. Now, if it is already computed and you re-run
## the previous command, you will get the output "getting cached data"
## before it returns the inverse. 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # initialize m as nothing
  m <- NULL
  # define internal functions
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # will return the matrix
  get <- function() x
  # will cache the inverse to whatever is supplied
  setinv <- function(inverse) m <<- inverse
  # will return the inverse (initialized to NULL)
  getinv <- function() m
  # store the functions in a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated (and 
##  the matrix has not changed), then the cacheSolve should retrieve the 
##  inverse from the cache.
cacheSolve <- function(x, ...) {
  # call internal function on makeCacheMatrix object. 
  m <- x$getinv()
  # If it exists, print message and value of m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if it doesnt exist (you don't hit the return function), continue and c
  # get the matrix
  data <- x$get()
  # compute the inverse
  m <- solve(data, ...)
  # then set the inverse 
  x$setinv(m)
  ## Return a matrix that is the inverse of 'x'
  m
}

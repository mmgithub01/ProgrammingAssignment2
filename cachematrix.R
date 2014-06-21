###############################################################################
## makeCacheMatrix: 
##  Creates a special "matrix" object that can cache its inverse.
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(invmatrix) { invx <<- invmatrix }
  getinvmatrix <- function() { invx }
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)

}

###############################################################################
## cacheSolve: 
##  computes the inverse of the "matrix" returned by makeCacheMatrix. 
##  If the inverse has already been calculated, then it retrieves 
##  the inverse from the cache.
###############################################################################
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invx <- x$getinvmatrix()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinvmatrix(invx)
  return(invx)
}

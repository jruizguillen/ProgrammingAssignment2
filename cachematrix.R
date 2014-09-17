## makeCacheMatrix creates the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL #Create variable I -for Inverse- and assign NULL value. 
  
  # Create set function that asigns y to x (from parent environment) and NULL to I
  set <- function(y) {
    x <<- y 
    I <<- NULL 
  }
  
  # Get x value from makeCacheMatrix environment 
  get <- function() x
  
  # Set I to inverse value (I is defined at makeCacheMatrix environment)
  setInv <- function(inv) I <<- inv 
  
  # Set I value from makeCacheMatrix environment
  getInv <- function() I
  
  # Return set 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function returns the inverse of x (original matrix)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  I <- x$getInv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInv(I)
  I
}

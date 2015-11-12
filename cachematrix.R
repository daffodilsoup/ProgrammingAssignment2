##makeCacheMatrix is a function that will create a matrix from 
##the numbers specified by the user.

makeCacheMatrix <- function(x=matrix()) {
  
  inv    <- NULL
  fset <- function(y) {
    
     x <<- y
     inv <<- NULL
    
  }
  
  fget     <- function() x
  setinv     <- function(w) inv <<- w
  getinv     <- function() inv
  list(set     = fset, 
       get     = fget,
       setinv = setinv,
       getinv = getinv)
}

##cacheSolve will either return a stored value for the 
##inverse of the matrix, or will compute a new one.
cacheSolve <- function(z) {
  
  p <- z$getinv()
  
  if(!is.null(p)) {
    
    message("Getting cached inverse...")
    return(p)
    
  }
  
  data <- z$get()
  
  p <- solve(data)
  
  z$setinv(p)
  
  p
  
}

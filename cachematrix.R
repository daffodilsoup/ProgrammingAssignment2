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

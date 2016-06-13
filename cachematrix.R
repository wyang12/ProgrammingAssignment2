## This file contains three functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting the cached inverse matrix")
    return(inv)
  }
  data <- x$get()

  if (f(data)){
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
   } else{
    print("This matrix is not invertible")
  }
  
}

## This function determines if a matrix is invertible or not 
f <- function(m) class(try(solve(m),silent=T))=="matrix"



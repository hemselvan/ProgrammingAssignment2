makeCacheMatrix <- function( x = matrix() ) {
  ## x is a matrix that is invertible
  ## set function is to set the matrix
  ## get function is to get the matrix
  ## setInverse function is to set the inverse of matrix
  ## getInverse function is to get the inverse of the matrix
  
  inv <- NULL ## Initialize inverse to null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x ## This function gets the matrix
  
  setInverse <- function(inverse) inv <<- inverse ## This function is to set the inverse of the matrix
  
  getInverse <- function() inv ## This function is to "get" the inverse matrix
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## x is the output of makeCacheMatrix function
  ## cachesolve returns the inverse of the matrix which is input to makeCacheMatrix function
  
  inv <- x$getInverse()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # then don't re-compute; instead get it from the cache
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, it means that the inverse is not in the cache, hence compute the inverse 
  mat.data <- x$get()
  inv <- solve(mat.data,...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setInverse(inv)
  
  inv
}
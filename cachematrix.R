## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse

## This function creates a special "matrix", which is really a list containing a function to set the value of the matrix;
## get the value of the matrix; set the value of the inverse; and get the value of the inverse

makeCacheMatrix <- function(x = matrix())   {
  inv <- NULL
  set <- function(y)  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function returns the inverse of the matrix, first checking to see if it is already cached

cachesolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
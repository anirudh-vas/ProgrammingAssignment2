## Using these two functions, we create a special object that stores a
## numeric matrix and cache's its inverse.

## This function creates a special "matrix" object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the stored inverse value to NULL
  inv <- NULL
  
  # set value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  
  # get value of matrix
  get <- function() x
  
  # set inverse of matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # get inverse of matrix
  getinverse <- function() inv
  
  # return a list containing all functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  The following function calculates the inverse of the special "matrix"
## created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  
  # getting the inverse
  inv <- x$getinverse()
  
  # if inverse already exists, check if it is already cached
  # if it is , return cached inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if it is not already cached, get matrix
  data <- x$get()
  
  # compute inverse of matrix
  inv <- solve(data, ...)
  
  # cache inverse of matrix
  x$setinverse(inv)
  
  # return inverse of the matrix
  inv
}

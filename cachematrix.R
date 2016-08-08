## This function creates a Matrix and it is stored so that it can be used by another function
## by setting the matrix with this function it can be called by typing the vartiable where the 
## the result is saved with $get.

## While the function doesn't have a solution saved in getinverse, it will be null. Once we
## run the function cacheSolve wi can call the solution by typing the variable $get inverse
## otherwise we can save a solution in this function with the variable$setinverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## when we have set the matrix in the other function, it is stored un the MakeCacheMAtrix$get
## when we set the matrix, if we set a solution, this function will see if we have one or not,
## if we have one it will return that solution, if not it will get the matrix and it will calculate
## the inverse of it, store it in the variable$setinverse and printing that solution.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setinverse(s)
  s
}


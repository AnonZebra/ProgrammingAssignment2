## Functions for a) storing and retrieving a matrix, and b) calculating and retrieving
## the inverse of said matrix.

## A function that produces a list of four functions, for 1) storing a matrix,
## 2) retrieving said matrix, 3) storing the inverse of said matrix and 
## 4) retrieving the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y) { # a)
    x <<- y
    i <<- NULL
  }
  get <- function() x # b)
  setinv <- function(inv) i <<- inv # c)
  getinv <- function() i # d)
  list(set = set, get = get,
       setinv = setinv, 
       getinv = getinv)
}


## This function checks to see if the passed argument/object has a cached inverse matrix
## object, otherwise it uses the 'get function' to retrieve the original matrix and
## calculate the inverse, and then returns it.

cacheSolve <- function(x, ...) {
  te <- x$getinv()
  if(!is.null(te)) {
    print('getting cached data')
    return(te)
  }
  te <- solve(x$get())
  return(te)
}

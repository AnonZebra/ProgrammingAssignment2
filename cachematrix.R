## Functions for a) storing and retrieving a matrix, and b) calculating and retrieving
## the inverse of said matrix.

makeCacheMatrix <- function(cachm = matrix()) {
  cachs <- NULL
  set_cachm <- function(y) {
    cachm <<- y
    cachs <<- NULL
  }
  
  get_cachm <- function() cachm
  
  set_cachs <- function(solvmat) {
    cachs <<- solvmat
  }
  
  get_cachs <- function() {
    return(cachs)
  }
  
  return(
    list(set_cachm=set_cachm, get_cachm=get_cachm,
         set_cachs=set_cachs, get_cachs=get_cachs)
  )
  
}

cacheSolve <- function(cmat) {
  sma <- cmat$get_cachs()
  if (!is.null(sma)) {
    message("getting cached data")
    return(sma)
  }
  sma <- solve(cmat$get_cachm())
  cmat$set_cachs(sma)
  return(sma)
}


## Put comments here that give an overall description of what your
## functions do

## Matrix created, can set inverse, and cache

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    
    x <<- y
    inv <<- NULL
    
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## retrieves inverse from previous functions cache, if already calculated, otherwises calculates

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  inv
}

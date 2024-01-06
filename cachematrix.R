## Put comments here that give an overall description of what your
## functions do

## Create a Cache Matrix
makeCacheMatrix <- function(x = matrix()) {
  CacheM <- NULL
  set <- function(y= matrix()) {
    x <<- y
    CacheM <<- NULL
  }
  get <- function() x
  setinverse <- function(y= matrix()) CacheM <<- y
  getinverse <- function() CacheM
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Check and use Cache Matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

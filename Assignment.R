# function makes the matrix and inverts it. 
makeCacheMatrix <- function(x = matrix()) {
  t <- NULL
  set <- function(y) {
    x <<- y
    t <<- NULL
  }
  get <- function() x
  setinverse<- function(solve) t <<- solve
  getinverse <- function() t
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#the function and calculates the inverse matrix

cacheSolve <- function(x, ...) {
  
  t <- x$getinverse()
  if(!is.null(t)) {
    message("getting cached data")
    return(t)
  }
  data <- x$get()
  t <- solve(data, ...)
  x$setinverse(t)
  t
}
## Programming Assignment 2
## Paresh Raote, zdprshr
##

## Returns a list containing functions that are setters and getters
## for the passed in matrix and it's cached inverse
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



## Takes a list object created using makeCacheMatrix function
## and retrieves the cached inverse from it, if not found, it 
## will compute and store it back into the list object
cacheSolve <- function(x, ...) {
  inv <-x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

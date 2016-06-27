## These functions create a special matrix capable of caching its inverse, and checking whether this inverse has 
## been previously computed to avoid unnecessarily computing it again

## makeCacheMatrix is a list of functions to create a special cacheMatrix (set), retrieve the cacheMatrix (get),
## store the inverse of this matrix as computed by cacheSolve, below (setinverse), and retrieve that inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of a cacheMatrix created above; first, however, it checks whether the inverse 
## has been calculated previously. if so, it returns this value without calculating again.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
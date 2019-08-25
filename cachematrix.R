## These functions, makeCacheMatrix and cacheSolve, are designed to
## create a matrix object, and cache its inverse.
## makeCacheMatrix creates the matrix, and caches its inverse
## cacheSolve computes the inverse, unless it already exists, in which case
## it fetches it from the cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of matrix x,
## if the inverse is already cached, fetch it
## if the matrix has changed or is not cached,
## then solve and return the inverse

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


## Testing Matrix below:
# m <- matrix(rnorm(9),3,3)
# m1 <- makeCacheMatrix(m)
# cacheSolve(m1)
# 
# m
# solve(m)

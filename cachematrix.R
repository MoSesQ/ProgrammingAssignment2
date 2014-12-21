## makeCacheMatrix takes input of matrix [c(elements), dimensions] we assume every given matrix is invertible
#stores matrix as the variable named by user when initialized
#stores inverted matrix as inv, after it has been computed by CacheSolve
#sets inverted matrix as inv, after it has been computed for the 1st time by CacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }  
  get <- function() x
  setinverted <- function(inverted) inv <<- inverted
  getinverted <- function() inv
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
}
## takes as input matrix named and created by makeCacheMatrix
#calls functions in makeCacheMatrix to determine if inv has been cached already, get the inv, or else get the matrix and compute the inverse
#returns the inverted matrix["inv"]
cacheSolve <- function(x, ...) {
  
  inv <- x$getinverted()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverted(inv)
  inv  
}

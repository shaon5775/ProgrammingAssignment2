
## This function creates a "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv1 <- NULL
  set1 <- function(y) {
    x <<- y
    inv1 <<- NULL
  }
  get1 <- function() x
  setInverse1 <- function(inverse) inv1 <<- inverse
  getInverse1 <- function() inv1
  list(set1 = set1,
       get1 = get1,
       setInverse1 = setInverse1,
       getInverse1 = getInverse1)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv1 <- x$getInverse()
  if (!is.null(inv1)) {
    message("getting  data")
    return(inv1)
  }
  mat1 <- x$get()
  inv1 <- solve(mat1, ...)
  x$setInverse(inv1)
  inv1

}

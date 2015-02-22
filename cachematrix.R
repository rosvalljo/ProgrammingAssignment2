# makeCacheMatrix creates a list containing a function to
# - set value of the matrix
# - get value of the matrix
# - set value of inverse of the matrix
# - get value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve function returns the inverse of the matrix by first checking if
# the inverse already exists. If (exists) then use cache and don't compute.
# Prerequisites: matrix is invertible. Matrix must be square
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  d <- x$get()
  i <- solve(d)
  x$setinverse(i)
  i
}

## Example exec:
## > x = rbind(c(1:2), c(2:1))
## > m = makeCacheMatrix(x)
## > cacheSolve(m)
## [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
## > cacheSolve(m)
## getting cached data.
## [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333

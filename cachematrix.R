## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {  
  mat.inverse <- NULL
  list(
    set = function(m) {
      mat <<- m
      mat.inverse <<- NULL
    },
    get = function() mat,
    set.inverse = function(inverse) mat.inverse <<- inverse,
    get.inverse = function() mat.inverse
  )
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(mat, ...) {        
  mat.inverse <- mat$get.inverse()
  if(!is.null(mat.inverse)) {
    message("getting cached data")
    return(mat.inverse)
  }
  matrix.data <- mat$get()
  mat.inverse <- solve(matrix.data, ...)
  mat$set.inverse(mat.inverse)
  mat.inverse
}

##makeCacheMatrix is a function that produces a matrix object which caches 
##its inverse, while cacheSolve produces inverse of the matrix object

##Function that produces a matrix object which caches its inverse
makeCacheMatrix <- function(matrix_object = matrix()) {
  inverse_object <- NULL
  
  set <- function(matrix) {
    matrix_object <<- matrix
    inverse_object <<- NULL
  }
  
  get <- function() {
    matrix_object
  }
  
  setInverse <- function(inverse) {
    inverse_object <<- inverse
  }
  
  getInverse <- function() {
    inverse_object
  }

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##Produces inverse of the matrix object
cacheSolve <- function(x, ...) {
  matrix_object <- x$getInverse()

  if(!is.null(matrix_object)) {
    message("getting cached data")
    return(matrix_object)
  }

  matrix_x <- x$get()

  matrix_object <- solve(matrix_x) %*% matrix_x

  x$setInverse(matrix_object)

  matrix_object
}

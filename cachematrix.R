## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix constructs the "matrix" that allows for
## its inverse to be cached.
makeCacheMatrix <- function(data = matrix()){
  mat <- NULL
  set <- function(y) {
    data <<- y
    mat <<- NULL
  }
  get <- function() data
  setmatrix <- function(matrix) mat <<- matrix
  getmatrix <- function() mat
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function

## cacheSolve generates the inverse from the special "matrix"
## and then caches the solution to be retrieved later.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$getmatrix()
  if(!is.null(mat)){
    message("getting cached matrix")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setmatrix(mat)
  mat
}

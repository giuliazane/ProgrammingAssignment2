## The two function below calculate the inverse of matrix x,
## unless this has already been calculated
## in which case it just reports the result of previous calculations


## The function makeCacheMatrix creates a list of functions
## including the orginal matrix of which we want to calculate the inverse
## and the function to be used to compute the inverse
## this function needs one argument: matrix x

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setmatrix<- function(solve) mat <<- solve
  getmatrix <- function() mat
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## The function cacheSolve returns the inverse of the matrix x
## if this has already been calculated it gets if from cache data
## The argument for this function is the output of the makeCacheMatrix function defined above

cacheSolve <- function(x, ...) {
  mat <- x$getmatrix()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setmatrix(mat)
  mat
}


## Small functions for creating and using inverted matrices which have caching ability

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # Let's check if we have correct input
  if (!is.matrix(x)) {
    stop("Please provide a matrix")
  }

  ## Init the inverse property
  inverted.matrix <- NULL

  ## Method to set the inverse of the matrix
  set <- function(y) {
     x <<- y
     inverted.matrix <<- NULL
  }

  # Functions for getting and setting cached inv. matrix value
  get <- function() x

  # Inversing the matrix using build in solve() function in R
  set.inverse <- function(solve) inverted.matrix <<- solve
  get.inverse <- function() inverted.matrix
  
  ## Returns the list of methods
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## getting a matrix that is the inverse of 'x'
  inverted.matrix <- x$get.inverse()

  ## returns if the inverse has already been calculated 
  if(!is.null(inverted.matrix)) {
    message("Getting cached matrix data")
    return(inverted.matrix)
  }

  ## Create inverted matrix in case there's no cached matrix available.
  
  ## getting the matrix from object
  matrix.2.inverse <- x$get()
 
  ## calc the inverse by using matrix multiplication
  inverted.matrix <- solve(matrix.2.inverse)

  ## storing the inverse to the object for future usage
  x$set.inverse(inverted.matrix)

  ## returning a matrix that inverse of 'x'
  inverted.matrix


}




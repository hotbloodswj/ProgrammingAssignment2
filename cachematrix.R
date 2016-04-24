## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
  ## mat : a square invertible matrix
  ## return : a list containing functions
  ##          setmatrix, getmatrix, 
  ##          set the inverse, get the inverse
  matrix_inversion <- NULL
  set <- function(original) {
    mat <<- original
    matrix_inversion <<- NULL
  }
  
  get <- function() mat
  setinversion <- function(inversion) matrix_inversion <<- inversion
  getinversion <- function() matrix_inversion
  list(set = set, 
       get = get, 
       setinversion = setinversion, 
       getinversion = getinversion)
}

cacheSolve <- function(mat, ...) {
  ## mat : output of makeCacheMatrix()
  ## return : inverse of the original matrix 
  
  matrix_inversion <- mat$getinversion()
  # if the inverse has already been calculated
  if (!is.null(matrix_inversion)) {
    message("getting cached data")
    return(matrix_inversion)
  }
  
  # calculates the inverse
  data <- mat$get()
  matrix_inversion <- solve(data, ...)
  
  # sets the value of the inverse in the cache
  mat$setinversion(matrix_inversion)
  matrix_inversion
}


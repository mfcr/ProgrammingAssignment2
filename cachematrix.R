## Put comments here that give an overall description of what your
## functions do

## here we have two functions. makeCacheMatrix and cacheSolver.
## with them we can obtain the inverse of a given matrix.

## Write a short comment describing this function
## makeCacheMatrix takes as argument a matrix and returns a list that contains 4
##functions that get and set the values of tha matrix and that get and set the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) x_inv <<- solve
  get_inverse <- function() x_inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)   ##list of functions returned

}


## Write a short comment describing this function
##cacheSolve takes as argument the matrix x and calculate the inverse using solve
##and sets the value of the inverse using set_inverse function of the list returned
##by the previous function if the inverse was not already set.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv <- x$get_inverse()
  if(!is.null(x_inv)) {  ##matrix already calculated.
    message("getting inverse of the given matrix")
    return(x_inv) ##inverse returned
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$set_inverse(x_inv)
  x_inv  ##inverse returned.
}

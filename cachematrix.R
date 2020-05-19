# This program illustrates the lexical scoping in R a powerful concept
# References
#   [1] https://cran.r-project.org/web/packages/matlib/vignettes/inv-ex1.html
#   [2] https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprogAssignment2Prototype.md
#   [3] https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/mowodw4rEemS0Q4U5mc4kg
#
# Assumptions
#    The matrix is inverible
# 
# Testing
#    a) Define an invertible matrix first.
#        o <- matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE)
#    b) Create a cached matrix This creates the helper functions
#        m <- makeCacheMatrix(o)
#    c) Invoke the matrix inversion
#        cacheSolve(m)
# Results
#    > source("cachematrix.R")
#    > m <- makeCacheMatrix(o)
#    > cacheSolve(m)
#    [,1]    [,2]   [,3]
#    [1,] 0.0625  0.0625  0.125
#    [2,] 0.6875 -0.3125 -0.625
#    [3,] 0.2500  0.2500 -0.500
#    > cacheSolve(m)
#    getting cached data
#    [,1]    [,2]   [,3]
#    [1,] 0.0625  0.0625  0.125
#    [2,] 0.6875 -0.3125 -0.625
#    [3,] 0.2500  0.2500 -0.500
# Common Errors
#    i) $ operator is invalid for atomic vectors
#       The helper functions have not been created i.e., you invoked
#       cacheSolve on the original matrix and not the cached matrix
#       so the helpler functions are not attached to the object
#    ii)  Error in solve.default(data, ...) :
#         Lapack routine dgesv: system is exactly singular: U[3,3] = 0 
#       The matrix is not invertible. The requirement is that the
#       starting matrix is invertible
# 
## ----------------------------------------------------------
#  makeCacheMatrix
#
#
#  Defines the cache matrix and the helper functions
## ----------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## ---------------------------------------------------------------------
#  Function: cacheSolve
#  
#  This function computes the inverse of the 
#  special "matrix" returned by makeCacheMatrix above. 
#
#  If the inverse has already been calculated 
#  (and the matrix has not changed), then cacheSolve will retrieve 
#  the inverse from the cache.
#
#
## ---------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # md <- as.data.frame(x)
  # m <- x[["getinverse"]]()
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

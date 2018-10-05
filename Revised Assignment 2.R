## The assignment contains two functions makeCacheMatrix and cacheSolve that caches the matrix inverse
##  makeCacheMatrix creates a matrix object which can cache the inverse of the input invertible square matrix 


makeCacheMatrix <- function(y = matrix()) {
  inv1 <- NULL
  s <- function(z) {
    y <<- z
    inv1 <<- NULL
  }
  g <- function() y
  sinv <- function(inverse) inv1 <<- inverse
  ginv <- function() inv1
  list(set = s, get = g, setinv = sinv, getinv = ginv)
}




##cacheSolve computes inverse of the matrix returned by the function ##makeCacheMatrix. The function retrieves the matrix unchanged if the matrix is ## unchanged and inverse already has been calculated



cacheSolve <- function(y1, ...) {
  ## Return a matrix that is the inverse of 'y'
  inv1 <- y1$getinv()
  if(!is.null(inv1)) {
    message("getting cached result")
    return(inv1)
  }
  data <- y1$get()
  inv1 <- solve(data, ...)
  y1$setinv(inv1)
  inv1
}


## Testing
## p <- matrix(rnorm(16),4,4)
## p1 <- makeCacheMatrix(p)
## cacheSolve(p1)


## [,1]       [,2]       [,3]       [,4]
## [1,] -0.1653269  0.2592203  0.6176218 -0.7520955
## [2,]  0.2828334 -0.1853499  0.4511382  0.2094365
## [3,]  0.1434840  1.0413868 -0.3550853 -0.3261154
## [4,]  0.1793583 -0.4252171 -0.4371493 -0.1749830


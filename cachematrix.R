##The function is aimed to compute the inverse of a square matrix
##can be done with the solve function in R. For example, if X is 
##a square invertible matrix, then solve(X) returns its inverse.

##This function creates a special "matrix" object
##that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #get the matrix
  get <- function() x
  #set the inverse matrix
  setinverse <- function(inverse) i <<- inverse
  #get the inverse matrix
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This function computes the inverse of 
##the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated 
##(and the matrix has not changed), 
##then the cachesolve should retrieve 
##the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##e.g.
##
##input1:
##m <- matrix(1:8, ncol = 2, nrow = 2)
##cm <- makeCacheMatrix(m)
##cacheSolve(cm)
##
##output1:
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
##
##
##input2:
##m <- matrix(1:2000000, nrow = 10000,ncol = 10000)
##for (i in 1:10000){m[i,i] <- 0}
##cm <- makeCacheMatrix(m)
##cacheSolve(cm)[1:3,1:3]
##
##output2:
##              [,1]          [,2]          [,3]
##[1,] -1.358534e-02  9.509522e-05  4.578663e-05
##[2,]  9.860194e-05 -9.997050e-05  4.578872e-09
##[3,]  4.929353e-05  4.754182e-09 -4.999021e-05
##
##
##
##input3:
##set.seed(1234)
##m <- matrix(runif(10000), nrow = 100,ncol = 100)
##for (i in 1:100){m[i,i] <- 0}
##cm <- makeCacheMatrix(m)
##cacheSolve(cm)[1:3,1:3]
##
##output3:
##          [,1]       [,2]      [,3]
##[1,] 0.0000000 0.03545673 0.6607546
##[2,] 0.6222994 0.00000000 0.5283594
##[3,] 0.6092747 0.28025778 0.0000000

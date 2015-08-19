## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

# create the matrix during the call of makeCacheMatrix()

a <- makeCacheMatrix(matrix(c(1,2,12,13), nrow =2, ncol = 2))

summary(a);
##set        1      -none- function
##get        1      -none- function
##setinverse 1      -none- function
##getinverse 1      -none- function

a$get();
##      [,1] [,2]
##[1,]    1   12
##[2,]    2   13


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}

cacheSolve(a)
##          [,1]        [,2]
##[1,] -1.1818182  1.09090909
##[2,]  0.1818182 -0.09090909


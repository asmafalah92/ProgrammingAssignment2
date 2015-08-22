## Programming assignment 2 (Lexical Scoping):
## Here's an overall description of what
## the functions do:
## makeCacheMatrix: This function creates a special
## "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been 
## calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## This function makeCacheMatrix creates a special "matrix",
## which is a list containing a function to
## *set the value of the matrix
## *get the value of the matrix
## *set the value of the inverse matrix
## *get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix(), ...) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list (set=set, get=get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function calculates the inverse of the 
## special "matrix" created with the above function.
## it first checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setinverse function

cacheSolve <- function(x = matrix(), ...) {
  s <- x$getinverse()
  if (!is.null(s)) {
    message("getting cached data")
    return (s)
  }
  mydata <- x$get()
  s <- solve(mydata, ...)
  x$setinverse(s)
  s
}

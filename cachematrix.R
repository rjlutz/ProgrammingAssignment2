## Created 17 May 2015, by rlutz for Coursera R Programming Course

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than computing it repeatedly. This 
# solution provides two methods: makeCacheMatrix() and cacheSolve().
# 
# makeCacheMatrix() is used to create a speical "matrix" object that can cache 
# its calculated inverse. cacheSolve() will either calculate the inverted matrix
# or return the cached solution if it is present. The inversion will only be 
# calculated during the first retrieval.


## these functions can be tested with accompanying tester.R file !

## makeCacheMatrix() initializes the custom object. Also provides mutator/accessor for inversion 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve() returns a matrix that is the inverse of 'x'. This function will return a cached instance,
## if it exists
cacheSolve <- function(x, ...) {
        
  i <- x$getinverse()
  if(!is.null(i))
    return(i)
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

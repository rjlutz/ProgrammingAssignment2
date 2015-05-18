## Created 17 May 2015, by rlutz for Coursera R Programming Course

# Tester for makeCacheMatrix() and cacheSolve(), provided in accompanying 
# cachematrix.R file

run <- function() {
  
  # functional tests, does the inversion really work? 
  
  result <- TRUE;
  
  m <- rbind(c(1,0,0), c(1,1,0), c(1,1,1)) # invertable
  result <- result & testDoubleInversion(m)
  result <- result & testIdentity(m)
  
  m <- diag(3) # identity, is invertible
  result <- result & testDoubleInversion(m)
  result <- result & testIdentity(m)
  
  m <- diag(102) # identity, is invertible
  result <- result & testDoubleInversion(m)
  result <- result & testIdentity(m)
  
  m <- rbind(c(1,0,0), c(0,0,1), c(0,1,0)) # involutory case, row pair interchanged
  result = result & testInvolution(m)    
  
  m <- rbind(c(1,0,0), c(0,-1,0), c(0,0,-1)) # involutory case, signature matrix
  result = result & testInvolution(m)    
  
  if (result) 
    print("Success!")
  else
    print("Fail!")
  
  
  # timing test: does the caching really work?
  
  m <- diag(2000) # identity, is invertible
  cached <- makeCacheMatrix(m)
  
  ptm <- proc.time()
  cacheSolve(cached) # first time
  t <- proc.time() - ptm
  print(paste("first inversion took: ", t[3])) # t[3] gets elapsed time 
  
  ptm <- proc.time()
  cacheSolve(cached)
  t <- proc.time() - ptm
  print(paste("second inversion took: ", t[3]))
  
}

# return true if a %*% cacheSolve(a) equals the identity matrix (with same dims as a)
testIdentity <- function(a) {
  b <- cacheSolve(makeCacheMatrix(a))
  matequal(diag(ncol(a)), a %*% b) 
}

# return true if cached inverse of a, when inverted, returns the original matrix
testDoubleInversion <- function(a) {
  b <- cacheSolve(makeCacheMatrix(cacheSolve(makeCacheMatrix(a))))
  matequal(a, b)
}

# return true if matrix a and inversion of matrix a are identical, this is an involutory case
testInvolution <- function (a) {
  cached <- makeCacheMatrix(a)
  matequal(a, cacheSolve(cached))
}

# convenience method to determine if two matrices are identical
matequal <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)

run()



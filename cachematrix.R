# These functions demonstrate how to write an R function that can cache potentially time-consuming computations.
# In this case, a matrix inverse is cached so that the expensive operation of computing the inverse is not repeated
# everytime it's inverse is needed.  It is computed the first time, cached and the cached inverse is returned
# in subsequent requests..
#
# example usage:
#
# > mymatrix = matrix(c(4,2,7,6),2,2)
# > mymatrix
#      [,1] [,2]
# [1,]    4    7
# [2,]    2    6
# 
# > cachedmatrix <- makeCacheMatrix(mymatrix)
# 
# > cacheSolve(cachedmatrix)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# > 
#
# > cacheSolve(cachedmatrix)
# getting cached inverse matrix
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# > 
#
# As can be seen above, the 2nd invocation of 'cacheSolve()' returns the cached inverse matrix (see the
# message 'getting cached ...' printed above) instead of recomputing the inverse again
#



# This funciton is used to cache a matrix and its inverse.  
#
#It accepts the given matrix, caches it and returns a list with 4 functions: 
#   set(matrix), get(), setinv(invmatrix), getinv().  
#
# The cached matrix can later be fetched using returnedlist$get().
# The cached inverse matrix can be set using returnedlist$setinv(inversematrix)
# The cached inverse matrix can be fetched using returnedlist$getinv()
#
makeCacheMatrix <- function(mtrx = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    mtrx <<- y
    inv <<- NULL
  }
  
  get <- function() mtrx
  
  setinv <- function(invmatrix) inv <<- invmatrix
  
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


# This function uses the cached matrix (using the above 'returned list') to 
#
# first check if the 'returnedlist' has a cached inverse matrix.  If so, it returns the cached inverse.
#
# if the cached inverse does not exist, it computes the inverse of the matrix.
# This inverse is stored in the above cache using returnedlist$setinv(inversematrix) function.  This
# stored inverse will then be returned by returnedlist#getinv() function.
#
cacheSolve <- function(cachematrix, ...) {
  ## Return a matrix that is the inverse of 'cachematrix'
  inv <- cachematrix$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  
  mtrx <- cachematrix$get()
  
  inv <- solve(mtrx)
  
  cachematrix$setinv(inv)
  
  inv  
}


## The functions below will create a false matrix (which in reality
## will be a list of functions) so that the original matrix we pass on
## and certain important information (in this case, the inverse of the
## matrix) can be manipulated, store and/or shown easily. The information
## will be stored on the cache so that computational time is saved.

## The makeCacheMatrix function will create the above mention false 
## matrix. 
## The list produced as a result will contain functions to set and get 
## the matrix and to set and get the inverse of the matrix. Please notice 
## that on the set function defined bellow, both the matrix and the 
## inverse are stored outside the global environment (use of the <<- 
## symbol).

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve 
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function will return the inverse of a matrix. Please
## notice the matrix that is expected here is in the form of the false
## matrix returned on the previous functions since we are going to be 
## calling functions from the abovemention list.
## It is important to see that, before cacheSolve calculates de inverse
## it checks if the inverse has already been calculated with the if loop.
## This simple step will save computation time and make the program more
## efficient.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}
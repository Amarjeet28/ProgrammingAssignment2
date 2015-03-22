##___________________________________________
##This function creates a special "matrix" object that can cache its inverse.

#Following function creates a list of functions which can be used to :
#Set the matrix if its already not in the cache
#Get that matrix
#Set the inverse of that matrix 
#Get the inverse of that matrix
##__________________________________________

makeCacheMatrix<- function(x=matrix()) {

## Initialize the value of the matrix "i" (used to store inverse) to NULL
  i <- NULL
  
  # Delcare another function "set where the value will be cached in 
  #1.Matrix is created for the first time. 2. changes made to cached matrix
  set <-function(y) {
    x<<-y
    ## change the value of inverse of the matrix in case the matrix was changed.
    i<<-NULL
  }
  ## gets the value of the inverse
  get <-function() x
  #calculates the inverse of non-singular matrix via the solve function
  setinverse <-function(inverse) i<<-inverse
  # gets the inverse
  getinverse <-function() i
  ## passes the value of the function makeCacheMatrix 
  list(set = set, get=get,setinverse=setinverse,getinverse=getinverse)
  
}

# Following function is used to compute the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#If the inverse has already been calculated (and the matrix has not changed), then the `cachesolve' retrieves the inverse 
#from the cache.

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  #if the inverse exists, it gets it.
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #if the inverse is not there, first it is calculated and then retrieved.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

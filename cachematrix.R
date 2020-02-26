end
## 

## makeCacheMatrix intializes getter, setter methods for Matrix (x) and
## its Inverse (inv)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## A value to store the inverse matrix is initialized

    ##This function sets x to a new matrix y, inv to NULL
    ##Function for setting Data
    set <- function(y){ 
         x <<- y 
        inv <<- NULL
      }

    ##Function for getting data
    get <- function() x 

    ##Function for setting/getting Inverse Matrix of X Matrix
    setinv <- function(inv1) inv <<- inv1 
    getinv <- function () inv
    
    #list to be used to call for the inverse matrix
    list (set = set, get = get, setinv=setinv, getinv = getinv)

}


## This function checks the cache for the Inverse of Matrix x. 
## If the cache holds the value of the inverse it is returned,
## if the cache is null or empty then a new inverse is collected
## and stored in the cache 

cacheSolve <- function(x, ...) {
 
  ##importing the inverse from the cache
   inv <- x$getinv() 
  
  ##Checking if the value needs to be computed again
  if (!not.null(inv)){
      message("getting cached data")
      return(inv)
  }
  
  ##Computing new inverse
  data <- x$get() 
  inv <- solve(data)
  x$setinv(inv)
  
  inv #return the value of inv
}

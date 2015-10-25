## This function culculates the inverse of a matrix and if the inversed of this matrix has already ## been culculated previously returns the results from the cache (where has store the results 
## from previous calculation).


## This function stores an inverse of matrix x in the variable 'xinv'.
## Then uses a set function to set a new matrix to the object has been created by the function ##'makeCacheMatrix'. Also uses the function 'get' which returns the matrix and the functions 
## 'setInv' and 'getInv' to set and get the inversed matrix accordingly. 

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL
 set <- function(z) {
    x <<- z
    xinv <<- NULL 
  }
  
  get <- function() x 
  setInv <- function(inv) xinv <<- inv
  getInv <- function() xinv 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## This function gets the inversed matrix from x and if is not null that means has been culculated ## previously, will return the result from cache. Otherwise will culculated the inverse and ##returns the result.

cacheSolve <- function(x, ...) {
  n <- x$getInv() 
  if(!is.null(n)) { 
    message("get data from cache")
    return(n) 
  }
  data <- x$get() 
  n <- solve(data) 
  x$setInv(n) 
  n 
}
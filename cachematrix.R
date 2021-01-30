## The makeCacheMatrix function creates a special object "invmatrix" with the ability to cache its inverse.
## The cacheSolve function calculates the inverse of the special object by name "invmatrix" returned by the previous function.

## The makeCacheMatrix function creates the variable defined as the inverse of a matrix x. The function then retrieves
## the value of the mtrix x and performs a computation to obtain the inverse of x, which is retrieved by the function getinv. 

makeCacheMatrix <- function(x = matrix()) {
  invmatrix<- NULL
  set<-function(y){
    x<<-y
    invmatrix<<- NULL
  }
  get<- function() x
  setinv<-function(inverse)
    invmatrix<<-inverse
  getinv<-function()invmatrix
  list(set=set, 
       get=get, 
       setinv=setinv, 
       getinv=getinv)
}


## The cacheSolve function defines invmatrix as the inverse of a matrix x. If invematrix represents a null value, then a message
## will appear where there is no data. X becomes inverted according to the invmatrix solution and is returned as a solution 
## when invmatrix is equal to the inverse matrix of a matrix x.

cacheSolve <- function(x, ...) {
  invmatrix<-x$getinv
  if(!is.null(invmatrix)){
    message("Retrieving cached data")
    return(invmatrix)
  } 
  else {
    matrix<-x$get
    invmatrix<-solve(matrix, ...)
    x$setinv(invmatrix)
    invmatrix
  }   
}

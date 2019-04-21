## This two functions aims to calculate and chache the inverse of a matrix.
## Such design will largely save some computations.


## This function aims to store the related message of a matrix 'x'.
## Note that the inverse of the matrix may not may not be calculated.
## The related operations will be done in 'cacheSolve' function.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){x<<-y;m<<-NULL}
  get<-function(){x}
  setinverse<-function(inverse){m<<-inverse}
  getinverse<-function(){m}
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function aims to print out the inverse of a matrix from the 'makeCacheMarix'.
## In the first few steps, it checks whether the inverse of a function has already been calculated.
## If yes, then it directly gives the inverse from the cached data and makes an announcement.
## Otherwise it claculates the inverse by using the 'solve()' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    print("Getting cached data.")
  }                               
  else{
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
  }
  m
}

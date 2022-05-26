## This is a function to cache an inverse matrix.
## This function generates a matrix containing a list of functions for setting  and getting 
## the matrix and the inverse one.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  get<-function() x
  setinverse <- function(inverse)  inv<<-inverse
  getinverse<- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function is to calculate the inverse of the matrix, but first
## to see if there is already calculated one in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){        
    message("getting cached data")   ## If there is no cached result, then
                                     ## return NULL, otherwise, already calculated one.
    return(inv)
  }
  inverse_matrix<-x$get()
  inv<-solve(inverse_matrix, ...)
  x$setinverse(inv)
  inv
}

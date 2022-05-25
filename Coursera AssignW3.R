makeVector<-function(x=numeric()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmean<-function(mean) m<<-mean()
  getmean<-function() m
  list(set=set, get=get,
       setmean=setmean,
       getmean=getmean)
}

cacheinverse<-function(x,...) {
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert<-x$get()
    inv<-solve(matrix_to_invert, ...)
    x$setinverse(inv)
    inv
}
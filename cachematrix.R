##the following functions aim to invert a matrix and cache the result

##this function inverts and caches a matrix "x"
makeCacheMatrix<-function(x=matrix()){
  ##i is a matrix the same size as x
  ##i is populated with zero
  i<-matrix(rep(0,times=nrow(x)^2),nrow(x),ncol(x))
  ##i_dummy indicates if the inverse was already computed
  i_dummy<-NULL
  set<-function(y){
    x<<-y
    i<<-matrix(rep(0,times=nrow(x)^2),nrow(x),ncol(x))
    i_dummy<<-NULL
  }
  get<-function()x
  setinverse<-function(solve)i<<-solve
  getinverse<-function()i
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

##this function computes the inverse of the "matrix" returned by makeCacheMatrix
##above. If the inverse has already been calculated, 
##then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  ## if i_dummy=NULL, it means the inverse wasn't computed yet  
  if(!is.null(i_dummy)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
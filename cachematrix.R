##This pair of funtions can cache the inverse of a matrix 

##makeCacheMatrix function creates a special matrix which could cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
}
get<-function() x
setinverse<-function(inverse) m<<-inverse
getinverse<-function() m
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

##cachSolve function returns a matrix that is the inverse of the input. 
##this function computes the inverse of the special matrix returned by makeCacheMatrix above.
##if the inverse of a square matrix has already been calculated, the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

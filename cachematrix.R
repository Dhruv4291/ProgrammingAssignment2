
## Function for creating a new matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmat<-function(inv) m<<- inv
getmat<-function() m
list(set=set, 
get=get,
   setmat=setmat,
   getmat=getmat)
}

## Function for computing the inverse of the matrix created by the above function

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmat()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix <- x$get() 
    m<-inv(matrix, ...)
    x$setmat(m)
    m
}

## Everything with the assumption that matrix is invertible
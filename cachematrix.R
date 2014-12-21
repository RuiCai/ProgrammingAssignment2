## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<-function(y){
     x<<-y
     m<<-NULL               ##set可以x<-y
  }
  get<-function() x 
  setsolve<-function(solve) m<<-solve     ##setsolve可以修改m<-solve
  getsolve<-function() m
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve) ##返回列表，以便调用

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m<-x$getsolve()
  if(!is.null(m)){                     ##判断是否已经有缓存值
    message("getting cached data")
    return(m)
  }
  
  data<-x$get()                  ##没有则计算逆矩阵
  m<-solve(data)
  x$setsolve(m)
  m
        ## Return a matrix that is the inverse of 'x'
}

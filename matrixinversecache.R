makeCacheMatrix<-function(matrixA=matrix()) {
  inverseB<-NULL
  set<- function(matrixB){
    matrixA<<-matrixB
    inverseB<<-NULL
  }
  get<-function()matrixA
  setinv<-function(inverse)inverseB<<-inverse
  getinv<-function()inverseB
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}
cacheSolve<-function(matrixA,...) {
  inverseB=matrixA$getinv()
  if(!is.null(inverseB)){
    message("getting cached data")
    return(inverseB)
  }
  mat.data<-matrixA$get()
  inverseB<-solve(mat.data,...)
  
  matrixA$setinv(inverseB)
  
  inverseB
}


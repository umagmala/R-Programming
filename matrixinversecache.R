# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

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
#function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

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


## The function takes a matrix-->x as its parameter 
## and returns a list 
## The list includes the original matrix, an empty matrix and the following 
## functions : setMatrix, getMatrix, setInverse and getInverse
## 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseM<-NULL
  
  setMatrix<- function(y) {
    x<<-y
    inverseM<<-NULL
  }
  
  getMatrix<-function(){
    x
  }
  
  setInverse<-function(solve){
    inverseM<<-solve
  }
  
  getInverse<-function(){
    inverseM
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function, takes the list "x" as an input parameter 
## It then checks if the inverse of the matrix is stored in cache
## If yes it then prints out the inverse of matrix x. Otherwise 
## it calculates the inverse using solve() function
## The new value is then stored in cache and can be recovered whenever
## Cachesolve is executed or through calling the x$getInverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseM<-x$getInverse()
  
  if(!is.null(inverseM)){
    message("Getting cached inverse Matrix")
    return(inverseM)
  }
  
  dataMatrix<-x$getMatrix()
  inverseM<- solve(dataMatrix,...)
  
  x$setInverse(inverseM)
  
  inverseM
}
## The function takes a matrix-->x as its parameter 
## and returns a list 
## The list includes the original matrix, an empty matrix and the following 
## functions : setMatrix, getMatrix, setInverse and getInverse
## 

makeCacheMatrix <- function(x = matrix()) {
  ##create a list containing the matrix and a set of functions to get the data 
  inverseM<-NULL 
  
  setMatrix<- function(y) {
    ## can be used to change matrix, inverseM is reset
    x<<-y
    inverseM<<-NULL
  }
  
  getMatrix<-function(){
    ## returns matrix
    x
  }
  
  setInverse<-function(solve){
    ## used to set inverse of the matrix
    inverseM<<-solve
  }
  
  getInverse<-function(){
    ## returns the inverse of matrix stored in cache
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
  
  ##calculate inverse of 'x'
  dataMatrix<-x$getMatrix()
  inverseM<- solve(dataMatrix,...)
  
  x$setInverse(inverseM) ## set inverse of 'x' in cache
  
  inverseM
}
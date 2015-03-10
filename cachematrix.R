## Put comments here that give an overall description of what your
## functions do

##Function makeCacheMatrix() makes special "matrix" that is actually a list
##containing 4 functions (set(),get(),setinverse() and getinverse())
##Function cacheSolve() takes special "matrix" as an argument and gets cached
## inverse that was previously calculated or calculates inverse of given
## matrix if inverse wasn't calculated previously

## Write a short comment describing this function
##Function makeCacheMatrix() takes matrix as argument and returns list that
##cotains 4 function. set() for setting value of matrix, get() for returning 
##the value of matrix, getinverse() for getting cached inverse of matrix and 
## setinverse() for setting value of inverse of matrix we begun with

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  
  get<-function() x
  
  setinverse<-function(inv){
    inverse<<-inv
  }
  
  getinverse<-function() inverse
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## Write a short comment describing this function
##Function cacheSolve() takes special 'matrix' as argument and
##returnes cached inverse of given special "matrix" if it exists(if it's not NULL)
##else it calculates inverse caches it and returns it

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()
  if(!is.null(inverse)){
    message('Getting cached data')
    return(inverse)
  }
  matrix<-x$get()
  inverse<-solve(matrix,...)
  x$setinverse(inverse)
  inverse   
        ## Return a matrix that is the inverse of 'x'
}

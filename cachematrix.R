## Put comments here that give an overall description of what your
## functions do

## This function will cache the inverse of a matrix after its
## inverse has been computed~~

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
      x<<-y
      inv<<-NULL
    }
    get<-function()x
    setinverse<-function(inverse) inv<<-inverse
    getinverse<-function()inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## When fed a matrix M<-makeCacheMatrix(matrix()), it will compute the inverse
## the first time cacheSolve(M) is used. This value will be stored in the list
## produced by the first function. If cacheSolve(M) is used again, it will 
## pull the inv value from the cache and great you with an uplifting message!

cacheSolve <- function(x, ...) {
    inv<-x$getinverse()
    if(!is.null(inv)){
      message("Retrieving the cached data, you glorious bastard!")
      return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setinverse(inv)
    inv
}

##TheProfessor712 01/10/2022

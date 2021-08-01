## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function creates a vector that sets the value of matrix x, 
## gets the value of x, sets the value of the inverse of x, and gets the 
## inverse of the matrix x.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get <-function() x
  setinverse <- function(solve) m<<-solve
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

## cacheSolve returns a matrix that is the inverse of the matrix x, either by 
## supplying the cached value if present, or computing the inverse if no cached 
## value is present.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


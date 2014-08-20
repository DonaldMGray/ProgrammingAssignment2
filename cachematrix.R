## R Programming 
## Assignment 2
## 
## This code supports caching the inversion of a matrix 
## to avoid the performance impact of recalculating an inverse everytime 
##
## These functions demonstrate the use of lexical scoping and the '<<-' operator
##
## Example usage:
##  mat<-matrix(0, nrow=4, ncol=4) #matrix with all zeros
##  lapply(c(1:4), function(i) mat[i,i]<<-2)    #set diagonal 
##  mcm<-makeCacheMatrix(mat)  #create special caching matrix
##  cacheSolve(mcm)     #and access (not cached first time)
##  cacheSolve(mcm)     #again - is cached 



## The makeCacheMatrix function optionally takes a matrix as input and creates a special 'matrix'
## which is really a list of four support functions
##    -- set the value of the matrix
##    -- get the value of the matrix
##    -- set the value of the matrix inverse
##    -- get the value of the matrix inverse
makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL # clear inverse when we are called
  set <- function(y) {  #set matrix
    mat <<- y
    inv <<- NULL  #and clear the inverse
  }
  get <- function() mat #get the matrix
  setinverse <- function(i) inv <<- i  #set inverse in the outer scope
  getinverse <- function() inv #get the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function takes a matrix function created by makeCacheMatrix and returns the matrix inverse.
## This will be cached if already computed for the matrix, else it will be calculated
cacheSolve <- function(x, ...) {
  inv <- x$getinverse() #get current inverse (will be cached if avail, else NULL)
  if(!is.null(inv)) { #return cached if not NULL
    message("getting cached data")
    return(inv)
  }
  #else, generate the inverse
  data <- x$get()  #get the matrix
  inv <- solve(data, ...)  #calc inverse
  x$setinverse(inv)  #and set it
  inv  #and return it
}

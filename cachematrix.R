## The makeCacheMatrix and cacheSolve functions together will 
## determine the inverse of a matrix with minimal computation
## time. Once an inverse is computed, it is stored in the cache
## because of lexical scoping rules, so that it does not
## have to be computed again.

## The makeCacheMatrix function creates an R object that stores a matrix 
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv<<-NULL
  }
  get <- function()x
  setinv <- function(inv2) inv <<- inv2
  getinv <- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## the cacheSolve function takes an object from makeCacheMatrix
## and either retrieves its inverse if already calculated, or 
## calculates it if not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinv(inv)
  inv
}

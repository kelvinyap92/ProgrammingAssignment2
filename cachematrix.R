#This function will create a matrix list that contains a function to perform step 1 - 2
#1. SET/GET matrix value
#2. SET/GET inverse Value
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <-function(y) {
      x <<- y
      inverse <<- NULL
    }
    get<- function() x
       setInverse <- function(inv) inverse <<- inv
       getInverse <- function() inv
       list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}

##Assume all matrix supplied is always invertible.
#cacheSolve function will return the inverse of the matrix sent over from makecacheMatrix function. If first argument
#is already inverse, it is already computed and can be retrieve from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
          message("getting cached data...")
          return(inverse)
        }
        
        args <- x$get()
        inverse <- solve(args)
        x$setInverse(inverse)
        inverse
}

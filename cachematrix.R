## Jooyoung Yu.

## To calculate large size matrix efficiently,
## This R code have two functions for cache inverse matrix

# load library to use inv function
library(matlib)

# This function returns matrix properties list which is used to cache inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(iv) i <<- iv
    
    getInverse <- function() i
    
    list(set=set, get=get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function checks if the inverse matrix is already calculated.
## It Calculate the inverse matrix at the first time, and then return cached result.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    data <-x$get()
    
    # calculate inverse matrix
    i<-inv(data) 
    
    x$setInverse(i)
    
    i
}



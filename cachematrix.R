## Assignment to learn how to use the <<- operator which can be used to assign
## a value to an object in an environment different from the current.
##
## Below are two functions that are used to create a special object that stores 
## a matrix and cache's its inverse. Since Matrix inversion is usually a costly
## computation there some benefits to caching the inverse of a matrix rather
## than compute it repeatedly.

## Function which creates an special object of a matrix that can be cached.
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    # Internal function to set the matrix and store it in the special object.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # Internal function to get the matrix or special object.
    get <- function(){
        x
    }
    
    # Internal function to set the value of the inverse of the matrix and 
    # store it in the special object.
    setinv <- function(inv){
        i <<- inv
    }
    
    # Internal function to get the value of the inverse of the matrix.
    getinv <- function(){
        i
    }
    
    # Return object of the function containing the calculated and stores values 
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## To compute the inverse of a matrix based on a current cache value returned
## from the makeCacheMatrix function.It first checks if the minverse value is
## cached before the calculation, to return or to calculated otherwise.
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    
    i <- solve(data, ...)
    x$setinv(i)
    i
}

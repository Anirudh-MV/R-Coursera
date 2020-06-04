## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function will accept a matrix and cache it and provide function to set and retrieve inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) {
                inverse <<- inv
        }
        getinverse <- function() inverse
        list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## Write a short comment describing this function
## This function will accept a makeCacheMatrix cache object and return it's inverse 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setinverse(inverse)
        inverse
}

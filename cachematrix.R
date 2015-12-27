
## R Programming Assignment 2 - Coursera

## To save computational resources in time-consuming computations
## caches can be used if what is being computed hasn't changed.

## In this example we are going to cache the inverse of a matrix
## to avoid that this costly computation is performed a second time
## if the matrix hasn't been changed.

## To do this we are first going to check if the computation,
## the inverse of the matrix, has already been done. If it hasn't, we will
## perform this computation and save it in the cache. If it has,
## we will return what has been saved in the cache.

## For this assignment, we assume that the matrix supplied is 
## always invertible.


## This function will save the inverse of a matrix in the cache
## It creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## the function makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve doesn't 
## perform the same computation again, that is, it retrieves the inverse
## from the cache. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    
    # Computing the inverse of a square matrix can be 
    # done with the solve function in R
    
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
    
}


# # -- test
# m1 <- matrix( c(2, 9, 3, 8, 5, 7, 1, 3, 9), nrow=3, ncol=3)
# m1
# m  <- makeCacheMatrix(m1)
# m$get()
# 
# # in the first run there is no cache yet, it must perform the computation
# cacheSolve(m)
# 
# # In the second run, the result will be retrived from the cache
# cacheSolve(m)


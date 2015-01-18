## Put comments here that give an overall description of what your
## functions do

## We define a function makeCacheMatrix to store a Matrix and its Inversed.
## Having that data stored saves us to recalculate data that
## can be long to calculate in large matrix.
## The function cacheSolve is the function that verifies if the matrix
## has its inverse stored. If not, the function calculates and stores it.

## We define a set of function to store and to get a matrix (set and get)
## and the inverse of that matrix (setmatrixinverse and getmatrixinverse)
## We obtain a Cached Matrix
##
##Example:
##mat <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
##cacheSolve(mat)
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrixinverse <- function(solve) m <<- solve
    getmatrixinverse <- function() m
    list(set = set, get = get,
         setmatrixinverse = setmatrixinverse,
         getmatrixinverse = getmatrixinverse)
}

## We try to get the inverse of a matrix
## If that data already exists, we don't calcute it again
## If it doesn't exist, we use the function solve() to 
## obtain the inverse of a matrix. After that, we store
## that matrix so afterwards we don't have to recalculate it.
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrixinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrixinverse(m)
    m
}

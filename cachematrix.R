## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##mat <- rbind(c(1, -1/4), c(-1/4, 1))
##mat2 <- makeCacheMatrix(mat)
##cacheSolve(mat2)
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

## Write a short comment describing this function

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

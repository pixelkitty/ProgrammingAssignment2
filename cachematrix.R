## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if inverse has been cached
## if so, retrieves cached inverse; otherwise solves

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        nr <- nrow(x)
        nc <- ncol(x)
        m <- x[1:nr,1:nc]
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}

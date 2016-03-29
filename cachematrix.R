## The following two functions cache the inverse of a matrix

## This function creates a list containing functions to 1)set the matrix, 2) get the matrix
        ## 3) set the inverse, and 4) get the inverse

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

## This function checks to see if inverse of matrix has been cached.
## If so, returns the cached inverted matrix. 
## Otherwise, computes the inverse and sets value of the inverted matrix in cache.

cacheSolve <- function(x = matrix, ...) {
        m <-x$getinverse()
        if(!is.null(m)){
                message("getting cached data...")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
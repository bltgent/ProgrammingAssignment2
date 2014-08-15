## This satisfies Assignment 2.  The functions allow a matrix to be inverted, cached, and called upon versus 
##computing the matrix repeatedly.

## The makeCacheMatrix function makes a matrix variable that can store the contents of a supplied matrix.
makeCacheMatrix <- function(x = numeric()) {
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
## This function checs for the existence of a cached matrix.  If found it returns it.  If not it creates it.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

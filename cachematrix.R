## The following functions create the environment "makeCacheMatrix" that allows you to cache a matrix, and retrieve and 
## inverse it without having to recalculate if a value already exists.  This is done without explicitly defining the
## data objects within each function.

## The following function creates a matrix, populating data objects 'x' and 'm' variables to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Checks 'm' to see if is null.  If not null, returns the matrix 'm', and returns the inverse.  
## If 'm' is null, grabs the value of 'x' from the global environment, and returns the inverse.
## This is possible because when makeCacheMatrix was run, it left pointers that allowed the
## environment to stay in memory, which can now be accessed by cachesolve.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
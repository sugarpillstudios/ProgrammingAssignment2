## These functions create a matrix (using makeCacheMatrix) with a cache-able inverse
## that can be calculated and retrieved (using cacheSolve)

## this function creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(newInverse) inverse <<- newInverse
    getInverse <- function() inverse
    
    list(set = set, get = get, setInverse=setInverse, getInverse=getInverse)
}


## this function retrieves (and calculates when necessary) that matrix inverse
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    # if(!is.null(inverse)) message("using cached matrix")
    if(is.null(inverse)) {
        # message("creating new cache")
        inverse <- solve(x$get())
        x$setInverse(inverse)        
    }
    inverse
}

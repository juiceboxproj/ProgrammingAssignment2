## create object mi, make "set" a function of y where y is superassigned to x
## create "get" as a function of x that creates the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setmatrixinverse <- function(inverse) mi <<- matrix
        getmatrixinverse <- function() mi
        list(set = set, get = get,
             setmatrixinverse = setmatrixinverse,
             getmatrixinverse = getmatrixinverse)
}
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getmatrixinverse()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setmatrixinverse(mi)
        mi
}

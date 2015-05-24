## Define some functions to get/set inverse of matrices

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x

        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## Caching the Inverse of a Matrix

cacheSolve <- function(x, ...) {

        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

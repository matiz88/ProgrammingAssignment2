# makeCacheMatrix creates a special “matrix” object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()

# checking to see if the inverse has already been calculated
        if(!is.null(m)) {
                message("getting cached data")
# getting the inverse from the cache and skips the computation
                return(m)
        }
# otherwise, calculating the inverse 
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
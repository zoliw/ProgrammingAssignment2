## With the following functions it is possible cache the potentially 
## time-consuming inverse matrix computation. It could be very useful
## especially if it has to be computed repeatedly (e.g. in a loop). 
## If the contents of a matrix are not changing, it may make sense 
## to cache the inverse matrix so that when we need it again, 
## it can be looked up in the cache rather than recomputed. 


## makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse.
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

## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
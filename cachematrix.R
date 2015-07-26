## CacheMatrix test for Coursera Class RPROG - 030
## Demonstrate understanding of scoping rules in R
## to run:
##
## > t  <- replicate(500, rnorm(500)) 
## > tm  <- makeCacheMatrix(t)
## > system.time(cacheSolve(tm))
## 
## Try again for the cached version

## makeCacheMatrix creates a special matrix object with the ability to cache the inverse of itself

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                
                # Important: when you set a new value the cache is always cleared
                m <<- NULL
        }
        get <- function() x
        
        # The main caching action happens here
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        
        # Make the functions of this 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## cacheSolve takes a matrix and determines its inverse, using a cached version if available.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        
        # Check to see if the inverse value has already been cached
        if (!is.null(inv)){
                return (inv)
        }
        
        # Inverse is not cached, grab the data and solve it.
        data <- x$get()
        inv <- solve(data, ...)
        
        # Set the newly generated inverse number into the cache and return the inverse matrix
        x$setinv(inv)
        inv
}

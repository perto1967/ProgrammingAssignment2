
# Matrix inversion is a costly computation 
# this functions optimizes computation caching the inverse of a matrix
# on first calculation. 


# Function makeCacheMatrix:
# creates a special kind of matrix which can include the cached inverse matrix
# (once calculated with function cacheSolve)
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


# Function cacheSolve:
# solves a matrix and caches the result 
# on the following calls returns the cached result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
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

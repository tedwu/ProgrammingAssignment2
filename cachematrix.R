## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    # need to redo solve when the matrix changed (set to other value)
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setSolve <- function(s) m <<- s
    getSolve <- function() m
    
    list(set = set,
         get = get,
         setSolve = setSolve,
         getSolve = getSolve
        )

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    
    data <- x$get()
    m <- solve(data)
    x$setSolve(m)
    m
    
}

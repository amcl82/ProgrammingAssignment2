## This pair of functions will cache the inverse of the created matrix


## This function creates a matrix and caches the data into a different environment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)  
}

## This function computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated, a message of 'getting cached data' will show
##    and it will retrieve the inverse from the cache. Otherwise it will show a message of
##    'calculating inverse' then do the calculation and store the computed inverse in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m) 
    }
    else
        message("calculating inverse")
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

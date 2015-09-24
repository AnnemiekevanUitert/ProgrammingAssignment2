# Goal of the function makeMatrix is to create a new matirx holding the inverse of a given matrix
# the inverse matrix is cached so it can be used in other functions
# Goal of the function cachaMatrix is to obtain the inverse matrix created by makeMatrix from cache, so
# that the solve function has not to be computed again

#Create and cache the inverse of a given matrix
makeCacheMatrix <- function(x = matrix()) {
# parameter should be a sqaure matrix
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
# compute the inverse of the given matrix
    setinv <- function(solve)  m <<- solve
# defene a function to query the inverse matrix
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# Pick up the cached inverse matrix, if poosible 
# and show it on the display

cacheSolve <- function(x, ...) {
# pick up the cached inverse matrix
    m <- x$getinv()
# if inverse matrix exists use it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
# if inverse matrix has been changed, compute it again
# using the origanal matrix (x)
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

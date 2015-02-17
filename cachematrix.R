## makeCacheMatrix sets up the list of functions and cacheSolve uses those
## functions to check to see if a matrix inversion has been calculated,
## if so, return it, if not, calculate it and then return it.

## This function creates a list of functions, which get/set the value of a matrix,
## or get/set its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## This function checks to see if the inverse of the matrix has already been
## calculated. If yes, it returns that value. If not, then it calculates the 
## inverse and then returns that value.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}

## The below functions perform matrix inversion, but use cache options to
## improve the efficiency of the operation when cached data is available.

# makeCacheMatrix is a function that creates a list containing several
# matrix functions.
# 
# Args:
#     x: A square invertible matrix
#
# Returns:
#     A list containing 4 functions that can be called.
#
#     makeCacheMatrix$set(y) takes a matrix y and sets the square matrix to
#         match y.
#     makeCacheMatrix$get() returns the value of the square invertible matrix.
#     makeCacheMatrix$setMatrix() takes the inverted matrix and stores it to m.
#     makeCacheMatrix$getMatrix() returns the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setMatrix <- function(invertedMatrix) {
        m <<- invertedMatrix
    }
    
    getMatrix <- function() {
        m
    }
    
    list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## cacheSolve is a function that calculates the inverse of the matrix 'x,'
## but it checks to see if it has already been calculated. If so, it gets 
## the inverse matrix from the cache and skips the computation.

cacheSolve <- function(x, ...) {
    m <- x$getMatrix()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    } else {
        data <- x$get()
        m <- solve(data)
        x$setMatrix(m)
    }
    m
}

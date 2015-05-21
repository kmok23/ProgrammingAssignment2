# The below functions perform matrix inversion, but use cache options to
# improve the efficiency of the operation when cached data is available.
#
# To test, the following can be run:
# > source("cachematrix.R")
# > A = matrix(c(1,1,1,3,4,3,3,3,4),3,3) # Creates a square invertible matrix
# > a <- makeCacheMatrix(A) # Creates a list of several matrix functions on A
# > cacheSolve(a) # Cache is empty, so the matrix inverse is generated
# > cacheSolve(a) # Inverse matrix taken directly from previous computation


makeCacheMatrix <- function(x = matrix()) {
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


cacheSolve <- function(x, ...) {
    ## cacheSolve is a function that calculates the inverse of the matrix 'x,'
    ## but it checks to see if it has already been calculated. If so, it gets 
    ## the inverse matrix from the cache and skips the computation.
    tempMatrix <- x$getMatrix()
    if (!is.null(tempMatrix)) {
        message("getting cached data")
        return(tempMatrix)
    } else {
        data <- x$get()
        tempMatrix <- solve(data)
        x$setMatrix(tempMatrix)
    }
    tempMatrix
}

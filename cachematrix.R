
## This function is used to create a special matrix object that can cache
## it's inverse in locally defined variable "inv".  It exposes a list of
## functions for accessing the matrix data and inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inv <<- i
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse= setInverse,
             getInverse = getInverse)
}


## This function makes use of the makeCacheMatrix function to get the inverse
## of a matrix. The matrix is assumed to be SQUARE.

cacheSolve <- function(x, ...) {
        
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        if (nrow(data) == ncol(data)) {
                message("Using solve")
                inv <- solve(data, ...)
        }
        else {
                message("Usage error: Matrix must be square")
        }
        x$setInverse(inv)
        inv
}

## This function can be used to test the functions cacheSolve 
## and makeCacheMatrix. It uses these 2 functions to get 
## get the inverse of matrix.  That inverse is then multiplied
## with the original matrix and the displayed results should
## be an identity matrix.

tester <- function(x, ...) {
        cmat <- makeCacheMatrix(x)
        imat <- cacheSolve(cmat)
        if (!is.null(imat)) {
                matId <- cmat$get() %*% imat
                print (matId)
        }
}

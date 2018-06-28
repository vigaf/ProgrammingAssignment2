## Put comments here that give an overall description of what your
## functions do.

## Write a short comment describing this function:
## This function creates a special "matrix" object that can cache its inverse.
## Step by step of the function: (1st) set the values of matrix, (2nd) get the values of matrix,
## (3rd) set the values of the inversed matrix, and (4th) get the values of the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                setMatrix <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                getMatrix <- function() x
                setInv <- function(solve) inv <<- solve
                getInv <- function() inv
                list(setMatrix = setMatrix, getMatrix = getMatrix,
                     setInv = setInv,
                     getInv = getInv)
}

## Write a short comment describing this function:
## This function computes the inverse of the matrix returned by 'makeCacheMatrix' function.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
                inv <- x$getInv()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$getMatrix()
                inv <- solve(data, ...)
                x$setInv(inv)
                inv
}

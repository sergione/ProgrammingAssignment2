## The following functions cache the inverse of a matrix. 
## The inverse will be either returned from the cache or 
## calculated (if the inverse has not been calculated before).

## The function makeCacheMatrix creates a cacheable matrix function. 
## It takes as argument a matrix and 
## it returns a function that contains sub-functions to
## get/set the initial matrix and get/set the inverse matrix
## Example: mat <- matrix(c(5, 6, 7, 8), 2, 2)
##          cachedMat <- makeCacheMatrix(mat)
makeCacheMatrix <- function(x = matrix()) {
    ## initially set the inverse the matrix to null
    inverseMatrix <- NULL
    
    ## function used to save the current matrix and set the inverse to null
    set <- function(y) {
      x <<- y
      inverseMatrix <<- NULL
    }
    
    ## function used to retrieve the initial matrix
    get <- function() x
    
    ## function used to save the inverse matrix
    setInverse <- function(inv) inverseMatrix <<- inv
    
    ## function used the retrieve the inverse matrix
    getInverse <- function() inverseMatrix
    
    ## the list of functions makeCacheMatrix returns
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inverse of a cacheable matrix.
## if the inverse was previously calculated, it will return the inverse from cache
## otherwise it will calculate the inverse, cache it, and return the inverse
## Example: mat <- matrix(c(5, 6, 7, 8), 2, 2)
##        cachedMat <- makeCacheMatrix(mat)
##        cacheSolve(cachedMat)
cacheSolve <- function(x, ...) {
    ## get the matrix inverse of x
    inverseMatrix <- x$getInverse()
    
    ## return the inverse if it was previously calculated
    if (!is.null(inverseMatrix)) {
        message("getting cached data")
        return (inverseMatrix)
    }
    
    ## if the inverse was not calculated
    ## save the matrix and the inverse
    ## and return the newly calculated inverse
    data <- x$get()
    inverseMatrix <- solve(data)
    x$setInverse(inverseMatrix)
    inverseMatrix
}

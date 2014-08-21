## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ComputedInverseMatrix <-NULL
        setOriginalMatrix <- function(y){
                x <<- y
                ComputedInverseMatrix <<- NULL
        }
        getOriginalMatrix <- function() x
        setInverseMatrix  <- function(inverseMatrix) ComputedInverseMatrix <<- inverseMatrix
        getInverseMatrix  <- function() ComputedInverseMatrix
        list (setOriginalMatrix = setOriginalMatrix, getOriginalMatrix = getOriginalMatrix,
              setInverseMatrix  = setInverseMatrix , getInverseMatrix  = getInverseMatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        
        ComputedInverseMatrix <- x$getInverseMatrix()
        if(!is.null(ComputedInverseMatrix)){
                message("getting cached data")
                return(ComputedInverseMatrix)
        }
        ComputedInverseMatrix <- solve(x$getOriginalMatrix())
        x$setInverseMatrix(ComputedInverseMatrix)
        ComputedInverseMatrix
}

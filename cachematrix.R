## These two functions together calculate the inversion of a matrix
## If the inversion is already stored in cache, then the value will be directly obtain
## If not, the inversion will be calculated and then stored in cache so that next time
## it could be directly called from the cache

## makeCacheMatrix takes a matrix as input and returns a list of four functions, which will
## do 1) set the matrix 2) get the matrix 3) set the inversion 4) get the inversion, respectively

makeCacheMatrix <- function(x = matrix()) {
        ivs <- NULL
        setMatrix <- function(y) {
                x <<- y
                ivs <<- NULL
        }
        getMatrix <- function() x
        setInversion <- function(inversionMatrix) ivs <<- inversionMatrix
        getInversion <- function() ivs
        list(setMatrix=setMatrix,getMatrix=getMatrix,
             setInversion=setInversion,getInversion=getInversion)
}


## cacheSolve takes output of makeCacheMatrix as input and test if the inversion has been store
## in cache. If yes, it will call directly from cache; if not, it will use the setinversion 
## from input to calculate the inversion and store it in cache so that won't need to calculate 
## next time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ivs <- x$getInversion()
        if (!is.null(ivs)){
                message("getting cached data")
                return(ivs)
        }
        inputMatrix <- x$getMatrix()
        ivs <- solve(inputMatrix)
        x$setInversion(ivs)
        ivs
}

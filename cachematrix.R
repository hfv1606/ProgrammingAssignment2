## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly


## UNIT TEST
##
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h3 <- hilbert(3)
## mcm = makeCacheMatrix(h3)
## cacheSolve(mcm)
## cacheSolve(mcm)

## UNIT TEST RESULTs
##
## > cacheSolve(mcm)
## [,1] [,2] [,3]
## [1,]    9  -36   30
## [2,]  -36  192 -180
## [3,]   30 -180  180
##
## > cacheSolve(mcm)
## using cached data
## [,1] [,2] [,3]
## [1,]    9  -36   30
## [2,]  -36  192 -180
## [3,]   30 -180  180


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(cacheMatrix = matrix()) {
    cachedInverse <- NULL
    
    set <- function(matrix) {
        cacheMatrix   <-  matrix
        cachedInverse <<- NULL
    }
    
    get <- function() {
        cacheMatrix
    }
    
    setInverse <- function(inverse){
        cachedInverse <<- inverse
    }
    
    getInverse <- function() {
        cachedInverse
    }
    
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- cacheMatrix$getInverse()
    
    if(is.null(inverse)) {
        inverse <- solve(cacheMatrix$get())
        x$setInverse(inverse)
    } else {
        message("using cached data")
    }
    
    inverse
}

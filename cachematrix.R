## Caches the inverse of a matrix
## The functions below create a matrix object that can cache its inverse,
## enabling it to be retrieved and reused without repeated computation

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL                                      ## set the value of the vector
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x                      ## get the value of the vector
        setInverse <- function(inverse) inv <<- inverse ## set the value of the inverse
        getInverse <- function() inv            ## get the value of the inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
                
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.     

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()                   ## return a matrix that is the inverse of 'x'
        if (!is.null(inv)) {                    ## check to see if the inverse has already been computed
                message("getting cached data")  ## if inverse already computed, retrieve it from the cache
                return(inv)
        }
        mat <- x$get()                           ## if inverse is not in cache, compute the inverse of the data
        inv <- solve(mat, ...)
        x$setInverse(inv)                       ## set the value of the inverse in the cache
        inv
}

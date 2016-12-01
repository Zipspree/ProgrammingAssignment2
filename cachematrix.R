## Caches the inverse of a matrix
## The functions below create a matrix object that can cache its inverse,
## compute the inverse of the matrix, and enable it to be retrieved and reused without repeated computation

## The function below creates the special matrix that caches its inverse:

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
                
## The function below determines the invest of the matrix created by makeCacheMatrix
## When the inverse has already been calculated, then it uses the cached inverse instead of recalculating it                

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

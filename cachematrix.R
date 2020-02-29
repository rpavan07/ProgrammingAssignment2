## Caching the inverse of a matrix:
## Below are functions that are used to create a special object that stores a matrix and caches its inverse
## This creates a matrix object.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
}
        get <- function() x
        Inverse0 <- function(inverse) inv <<- inverse
        Inverse1 <- function() inv
        list(set = set, get = get, Inverse0 = Inverse0, Inverse1 = Inverse1)


## This function finds out the inverse of the matrixcreated by the function above.

cacheSolve <- function(x, ...) {
        inv <- x$Inverse1()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$Inverse0(inv)
        inv
        }
        

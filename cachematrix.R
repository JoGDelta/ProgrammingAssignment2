## Two functions will be written
## makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse
## cacheSolve: this function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and hasn't changed) then cacheSolve will retrieve the
## inverse from the cache

## the makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                  ## initializing inverse as NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}                        ## function to get matrix x
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}               ## function to inverse of matrix x
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## this function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and hasn't changed) then cacheSolve will retrieve the
## inverse from the cache

cacheSolve <- function(x, ...) {               ## gets cache
        inv <- x$getInverse()
        if(!is.null(inv)) {                          ## checking whether inverse is null
                message("getting cached data")
                return(inv)                                ## returns inverse value
        }
        mat <- x$get()
        inv <- solve(mat, ...)                       ## calculates inverse value
        x$setInverse(inv)
        inv                                         ## returns a matrix that is the inverse of 'x'
}
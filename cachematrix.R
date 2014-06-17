## makeCacheMatrix and cacheSolve make use of the lexical scoping feature in R.
## The two functions cache the inverse of a matrix

## makeCacheMatrix creates a matrix containing a list of functions to
## set, get, set the inverse, get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                matrix <<- y
                inv <- NULL
        }
        get <- function() matrix
        setInverse <- function(i) inv <<- i
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve calculates the inverse of a matrix created by makeCacheMatrix above.
## It assumes that the matrix can be inverted.
## It checks to see if the inverse of the matrix has already been calculated.
## If so it retreives the inverse from cache. Otherwise it calcualtes the inverse by calling the "solve" function.
## Then it stores the inverse in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null( inv)) {
                message("getting cached data")
                return(inverse)
        }
        matrix <- x$get()
        inv = solve( matrix)
        x$setInverse( inv)
        inv
}
## The function 'makeCacheMatrix'
## creates a list containing
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse of the matrix
##      4. get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setmatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}

##  The following function calculates
##  the inverse of the matrix from the function above.
##  It checks if the inverse has already been calculated.
##  If yes, it skips the calculation and returns the inverse from cache.
##  Otherwise, it calculates the inverse and sets the value in cache.

cacheSolve <- function(x = matrix(), ...) {
    m <- x$getinverse()
    if(!is.null(m)) {       ## checks if the inverse has already been calculated
        message("getting cached data")
        return(m)           ## skips the calculation and gets the inverse from cache
    }
    data <- x$getmatrix()
    m <- solve(data, ...)   ## calculates the inverse of the matrix
    x$setinverse(m)         ## sets the value of the inverse in the cache
    m
}


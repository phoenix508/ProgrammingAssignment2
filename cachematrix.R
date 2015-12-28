## The combination of these two functions will be used to calculate the inverse of given matrixes 
## If the inverse has already been calculated then the second function will retrieve the inverse  
## from the cache

## The first function creates a matrix which contains a function to cache its inverse by mimicing the mean example

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function will calculate the inverse after checking the inverse has not been caulculated
## solve() function is used for calculating the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

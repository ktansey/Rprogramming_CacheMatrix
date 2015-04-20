
# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.

# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

# makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to: 
    # setMatrix = set the value of the matrix
    # getMatrix = get the value of the matrix
    # cacheInverse = set the value of the matrix
    # getInverse = get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrix <- function(solve) m <<- solve
    getMatrix <- function() m
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)

}

# cacheSolve calculates the inverse of the special "matrix" created with 
# makeCacheMatrix. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse matrix of the data and 
# sets it in the cache via the setMatrix function.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    m <- x$getMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setMatrix(m)
    m
    
}


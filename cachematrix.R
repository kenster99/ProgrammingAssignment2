# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    matinv <- NULL
    set <- function(y) {
        x <<- y
        matinv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matinv <<- inverse
    getinverse <- function() matinv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    matinv <- x$getinverse()
    if(!is.null(matinv)) {
        message("getting inverse from cache")
        return(matinv)
    }
    data <- x$get()
    matinv <- solve(data)
    x$setinverse(matinv)
    matinv      
}

## Matrix inversion is usually a costly computation.
## 'makeCacheMatrix' and 'cacheSolve' will help you create a matrix,
## then compute its inverse and cache the result.
##
## Usage
##
## m <- matrix(c(7, 4, 9, 3), nrow = 2, ncol = 2)
## cm <- makeCacheMatrix(m)
## cacheSolve(cm)


makeCacheMatrix <- function(x = matrix()) {
    ## Create and return a special matrix that can cache its inverse.

    ## 'x' is an invertible matrix

    # Cached inverse
    i <- NULL

    set <- function(y) {
        ## Store the matrix 'y' and invalidate cache
        x <<- y
        i <<- NULL
    }

    ## Get the matrix
    get <- function() x

    ## Store the matrix inverse in cache
    setinverse <- function(inverse) i <<- inverse

    ## Get the matrix inverse from cache or NULL if not cached
    getinverse <- function() i

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    ## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
    ## If the inverse has already been calculated
    ## (and the matrix has not changed), then the the inverse
    ## will be retrieved from the cache.

    ## 'x' is an invertible matrix returned by makeCacheMatrix

    # Retrieve inverse from cache
    i <- x$getinverse()
    if (!is.null(i)) {
        # Cache hit
        message("Getting the result from cache")
        return(i)
    }

    # Cache miss, retrieve the matrix
    matrix <- x$get()

    # Solve it
    i <- solve(matrix, ...)

    # Store result in cache
    x$setinverse(i)

    # Return inverted matrix
    i
}

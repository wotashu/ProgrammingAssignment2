## Put comments here that give an overall description of what your
## functions do
## Store in cache the inverse of a matrix.


## Write a short comment describing this function
## This function creates a list of functions that
## can set and pull from cache.

makeCacheMatrix <- function(x = matrix()) {
        # Send to cache a matrix and its inversion
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<-solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve will apply the matrix inversion 
## onto the matrix contained within the list
## supplied to it as created from makeCacheMatrix

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

## Example running of functions with a 2x2 example matrix
aMatrix <- makeCacheMatrix(matrix(c(1,2 ,12,13), nrow = 2, ncol=2, 
                                  byrow=TRUE))
aMatrix$get()
cacheSolve(aMatrix)
aMatrix$getinverse()

## This function creates a special "matrix" object that can cache its inverse.
## functions: Caching the Inverse of a Matrix
## Usage: makeCacheMatrix(x)
##        cacheSolve(x)

## prepare the matrix
## input: a square matrix
## output: a list
##         - set() # set matrix
##         - get() # get matrix
##         - setinverse() # set inverse matrix
##         - getinverse() # get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        # initial m as null
        m <- NULL
        # set matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get matrix
        get <- function() x
        # set inverse matrix
        setinverse <- function(inverse) m <<- inverse
        # get inverse matrix
        getinverse <- function() m
        
        # output as a list
        list(set=set,get = get,setinverse=setinverse, getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get the getinverse of x
        m <- x$getinverse()
        # if m is not null, getting cached data
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # other will calculate the inverse of x
        # get data
        data <- x$get()
        # calculate the inverse of data
        m <- solve(data,...)
        # assign inverse of data to x$setinverse
        x$setinverse(m)
        # return inverse
        m
}

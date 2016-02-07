## Functions are used to cache the inverse of a matrix

## The 'makeCacheMatrix' function creates a special
## "vector" which is a list of the following functions:
## 1) set - sets the value of the matrix
## 2) get - gets the value of the matrix
## 3) setinverse - sets the inverse of the matrix
## 4) getinverse - gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The 'cacheSolve' function first checks to see if the
## inverse of the matrix has already been calulated and
## cached. If it has, it will retrieve the inverse from
## the cache and skip the calculation. If not, it will
## calculate the inverse and set the value of the inverse
## in the cache using the 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        x <- x$get()
        i <- solve(x, ...)
        x$setinverse(i)
        i
}

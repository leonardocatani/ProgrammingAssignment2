## The two functions cache the inverse of a matrix

## this function make a matrix that can accomodate the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##The function compute the inverse of the matrix built with "makeCacheMatrix". 
## If the inverse has already been calculated, the function take the values from the cache
##  without repeating the computation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <-x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


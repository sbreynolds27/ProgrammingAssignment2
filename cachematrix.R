## Together, the functions makeCacheMatrix and cacheSolve can be used to create 
## a square matrix, compute its inverse, and cache the result.

## makeCacheMatrix creates a special matrix and defines four functions: Set()
## assigns the input argument x to the parent environment and assigns a NULL
## value to i in the parent environment. Get() assigns a getter to retrieve
## the x value from the parent environment. Setinverse() sets a value, inverse,
## for the inverse, i, in the parent environment. Getinverse() retrieves the 
## inverse value, i, from the parent environment. Finally, the list function 
## assigns names to each of the four functions to allow for the use of the $
## operator to access the functions.

makeCacheMatrix <- function(x = matrix()) { 
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## First, cacheSolve calls the getinverse() function and checks if it is null.
## If it is not null, the inverse value, i, is retrieved from the cache and 
## returned. If it is null, cacheSolve computes the inverse of the matrix, x,
## assigns the result to the object i, and caches it in the parent environment. 
## Last, cacheSolve returns the inverted matrix, i. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

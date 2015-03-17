## These functions will make it possible to cache the inverse of a matrix, so 
## it will be less time-consuming to repeatedly calculate the same inverse of
## a matrix. There are two functions, makeCacheMatrix, the function that creates
## an a special "matrix" object, that can cache its inverse, and the cacheSolve
## function, that computes the inverse of a matrix and retrieve it from the 
## cache if it has already been calculated.

## This function will create the special "matrix" object, in which it is possible
## to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function will compute the inverse of a special "matrix" object, 
## but first checks wether it is already calculated and included in the cache
## and if so, retrieves it from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                ## if the cache contains an inverse, it will send a message
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

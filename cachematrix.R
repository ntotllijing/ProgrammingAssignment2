## Caching the Inverse of a Matrix

## Creates a special "matrix" object that can cache its inverse
# 1.set the value of the vector (see "set <-")
# 2.get the value of the vector (see "get <-")
# 3.set the value of the inverse of the matrix (see "setsolve <-")
# 4.get the value of the inverse of the matrix (see "getsolve <-")

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        }
}


## calculates the inverse of the special matrix created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # cache isn't filled yet
        data <- x$get()       # get it
        m <- solve(data, ...) # solve it
        x$setsolve(m)	      # set it
        m		      # return it
}

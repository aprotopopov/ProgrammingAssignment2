
# Below are two functions that are used to create a special object that 
# stores a matrix and cache's its inverse matrix

# function makeCacheMatrix return list with functions:
# set - set the value of the Matrix
# get - get the value of the Matrix
# setInverse - set the value of the inverse Matrix
# getInverse - get the value of the inverse Matrix

# The cacheSolve function calculates the inverse of the special 
# "matrix" created with the makeCacheMatrix function. However, it first 
# checks to see if the inverseMatrix has already been calculated. If so, 
# it gets the inverseMatrix from the cache and skips the computation. 
# Otherwise, it calculates the inverseMatrix of the data and sets the 
# value of the inverseMatrix in the cache via the setmean function.


# function makeCacheMatrix provide methods for setting and getting matrix
# and inverseMatrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseMatrix) m <<- inverseMatrix
        getInverse <- function() m
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


# The cacheSolve function calculates the inverse of the special 
# "matrix" ans sets the value to "matrix" if it's not cached. Otherwise
# return cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                # return(m)
                m
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix involves several commands used to set, get, 
## and solve for the inversion of a Matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## set the matrix arguements
    set <- function(y=matrix()) {
        x <<- y
        m <<- NULL
    }
    ## get matrix arguements 
    get <- function() x
    ## functions used in cacheSolve 
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    ## list of commands created by makeCacheMatrix
    list(set = set, get = get, setsolve = setsolve,
                getsolve = getsolve)  
}


## Write a short comment describing this function

## Solve for the inverse of a matrix, if the matrix is the same, 
## use cached inverse matrix

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

}


## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix involves several commands used to set, get, 
## and solve for the inversion of a Matrix and will be used in 
## cacheSolve funtion listed below. The <<- operator is used 
## to assign a value to an object in an environment 
## that is different from the current environment. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## set the matrix arguments
    set <- function(y=matrix()) {
        x <<- y
        m <<- NULL
    }
    ## get matrix arguments 
    get <- function() x
    ## functions used in cacheSolve 
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    ## list of commands created by makeCacheMatrix
    list(set = set, get = get, setsolve = setsolve,
                getsolve = getsolve)  
}


## Write a short comment describing this function

## The following function calculates the inverse of a matrix created using 
## makeCacheMatrix (above). The function, cacheSolve, will first check to 
## see if an inverse of the matrix has already been calculated. If so, it 
## gets the inverse from the cache value and skips the computation.  

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    ## if inverse exist skip computation and return
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## gets matrix to br inverted
    data <- x$get()
    ## inverts matrix
    m <- solve(data, ...)
    ## Set the inverse value
    x$setsolve(m)
    m
}

}


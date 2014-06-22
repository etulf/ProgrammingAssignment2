## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix and cacheSolve cache the inverse of a matrix

## Write a short comment describing this function
## makeCacheMatrix gets and sets the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Write a short comment describing this function
## cacheSolve returns the inverse of the matrix x where x is a
## matrix from makeCacheMatrix

cacheSolve <- function(x,...) {
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

## mat<-matrix(c(1,2,3,2),2,2)
## solve(mat)
## cacheSolve()
## makeCacheMatrix()

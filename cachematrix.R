## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix implements matrix object with cached matrix inverse
## takes as input a matix x (assumed to be invertible)
## returns a list of methods:
##   set allows the matrix to be reassigned
##   get returns the matrix
##   setinv stores the inverse matrix for future reference
##   getinv returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y = matrix()) {
        x <<- y
        inv <- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## returns the inverse of makeCacheMatrix x
## takes as input a makeCacheMatrix
## returns the inverse either by loading the cached result stored in x
## or by computing the inverse directly and then storing the result in x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

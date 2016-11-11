## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function, makeCacheMatrix creates a special "vector", 
# which is really a list containing a function to

#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse of the matrix
#4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## Write a short comment describing this function
# This function gets the above function -- list of 4 different tiny function
#-- as the input. If the matrix is unchanged inside the makecacheMatrix it 
# returns the so called cached data (inverse of matrix) which may be computed
# before. If the matrix in makecacheMatrix has changed then it compute the
# the inverse, save it in the cacheMatrix and return the new value of inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}

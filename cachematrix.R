## This file contains functions that can be used to calculate the 
## inverse of a matrix.  The unique feature here is that these 
## functions cache the inverse and check the cache before 
## calculating the inverse.

## makeCacheMatrix takes a matrix and returns a list with fuctions
## that can be used to store the inverse of the original matrix
makeCacheMatrix <- function(x = matrix()) {

    #Create a variable to hold a reference to the inverse matrix
    invMatrix <- NULL

    # Setter for original matrix
    set <- function(y) {
        # Note we are using the superassingment operator here, so 
        # x and will will be updated in the enclosing frame where
        # they were defined
        x <<- y
        invMatrix <<- NULL
    }
    ## Getter for original matrix
    get <- function() x

    # Setter for inverse matrix
    setInverse <- function(newInvMatrix) invMatrix <<- newInvMatrix
    # Getter for inverse matrix
    getInverse <- function() invMatrix

    #Return a list containing references to these functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve takes an object of the form used by makeCacheMatrix
## and returns the inverse of that matrix.  It checks for a cached 
## inverse and uses that if it exists.
cacheSolve <- function(x, ...) {
    # Get the currently stored inverse
    inv <- x$getInverse()

    # Check to see if the stored inverse is NOT null
    if(!is.null(inv)) {
        # We have a non-null inverse so use it
        message("getting cached inverse")
        return(inv)
    }

    #The cached inverse was null, so calculate and store the inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)

    #Return the inverse
    inv
}

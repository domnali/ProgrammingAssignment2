## The first function makeCacheMatrix takes as a parameter a matrix and 
## creates a 'special matrix' which is a list with 4 elements:
##      set, function that gives value to the matrix from upper environment
##      get, function that returns the value of the matrix
##      setInverted, function that gives value to the inversion of the matrix from upper environment
##      getInverted, function that returns the value of the inversion of the matrix
##
## The second function cacheSolve, takes as a parameter a 'special matrix' and
## returns the inversion of this matrix


## function makeCacheMatrix : creates a list with 4 elements for a matrix and its invertion
## @param x : a matrix
## return : a list with 4 elements - functions

makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL
        ## gives value to the matrix from upper environment
        set <- function(aMatrix) {
                x <<- aMatrix
                inverted <<- NULL
        }
        ## returns the value of the matrix
        get <- function() x
        ## gives value to the inversion of the matrix from upper environment
        setInverted <- function(aInverted) inverted <<- aInverted
        ## returns the value of the inversion of the matrix
        getInverted <- function() inverted
        ## returns a list with 4 elements
        list(set = set, get = get,
             setInverted = setInverted,
             getInverted = getInverted)
}


## function cacheSolve : calculates the invertion of a matrix of get it if there is already calculated, and returns it.
##                       makes the assumption that the matrix is invertible.
## @param x : a list which is a 'special matrix'
## return : a matrix which is the inversion of the given matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverted <- x$getInverted()
        ## checking if the inverted matrix is already calculated
        if(!is.null(inverted)) { ## if so, return the inverted matrix
                message("getting cached data")
                return(inverted)
        }
        ## calculation of the inverted matrix with function solve()
        data <- x$get()
        inverted <- solve(data, ...)
        x$setInverted(inverted)
        ## return the inverted matrix
        inverted        
}

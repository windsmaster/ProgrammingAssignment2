
#####################################################################################################
## The following two functions implement a simple caching mechanism of the inverse of a matrix.
## Function <makeCacheMatrix> is used to create a "matrix" object with cache.
## Function <cacheSolve> is used to compute/retrieve the inverse matrix and to save it to cache.
##
## Usage example:
## > m <- matrix(1:4, nrow=2, ncol=2)
## > mc <- makeCacheMatrix(m)
## > mc$getInverse()  # NULL
## > cacheSolve(mc)   # computes inverse matrix for the first time
## > mc$getInverse()  # cached inverse matrix is now present
## > cacheSolve(mc)   # returns inverse matrix from cache
#####################################################################################################

##
## This function creates a special "matrix" object that can cache its inverse.
## The cache is reset if the original matrix gets changed.
##
makeCacheMatrix <- function(x = matrix()) {
        # Variable for storing cached inverse of 'x', initialize to NULL
        inverse.x <- NULL
        
        # Function which allows to replace the original matrix with another matrix
        set <- function(y) {
                x         <<- y     
                inverse.x <<- NULL  # reset cached inverse since original matrix changed
        }
    
        # Function that returns the original matrix 'x'
        get <- function() x
        
        # Function that takes provided matrix and stores it as cached inverse of 'x'
        setInverse <- function(solved_matrix) {
                inverse.x <<- solved_matrix
        }
        
        # Function that returns the cached inverse matrix of original matrix 'x'
        getInverse <- function() inverse.x
        
        ## Return a list of functions declared above
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

##
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the cache of the inverse is present, then cacheSolve will retrieve it.
##
cacheSolve <- function(x, ...) {
        # Try getting cache
        inverse <- x$getInverse()
        
        # If there's a cached inverse, return it right away
        if (!is.null(inverse)) {
                message("(cached result returned)")
                return (inverse)
        }
        
        # No cache was found, so get the original matrix and invert it
        matrix  <- x$get()
        inverse <- solve(matrix, ...)
        
        # Save inverse matrix to cache
        x$setInverse(inverse)
        
        ## Return a matrix that is the inverse of 'x'
        return (inverse)
}

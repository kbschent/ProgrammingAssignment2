## Programming Assignment 2: Lexical Scoping

## These 2 functions (makeCacheMatrix and cacheSolve) aid in calculating the
## inverse of a solvable matrix. Matrix inversion can take significant time to
## compute so the functions cache the inversed matrix so it can be recalled 
## without calculating again.


## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        # Initialize cache to null
        inversedMatrix <- NULL
        
        ## Sets the the matrix in the working environment
        set <- function(y) {
                x <<- y
                inversedMatrix <<- NULL
        }
        
        ## Gets the matrix
        get <- function() x
        
        ## Sets the inverse of the matrix
        setInverse <- function(solve) inversedMatrix <<- solve
        
        ## Gets the inverse of the matrix
        getInverse <- function() inversedMatrix
        
        ## Returns the functions to the calling environment
        list (set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not
## changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Check if inverse has been calculated and stored in cache
        inversedMatrix <- x$getInverse()
        
        ## Return the cached matrix if it is already stored
        if(!is.null(inversedMatrix)) {
                message("Getting cached matrix")
                return(inversedMatrix)
        }
        
        ## Get the matrix (if it isn't cached)
        matrix <- x$get()
        
        ## Find the inverse and store as newCache
        inversedMatrix <- solve(matrix, ...)
        
        ## Cache the inverse matrix
        x$setInverse(inversedMatrix)
        
        ## Return the inverse matrix
        inversedMatrix
}

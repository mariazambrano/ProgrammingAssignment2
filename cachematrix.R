## This script contains two functions that together provide a
## mechanism to calculate and cache the inverse of an invertible
## matrix. To calculate an inverse matrix, first call makeCacheMatrix()
## passing a source invertible matrix as a parameter. This function 
## will return a matrix wrapper object that can then be passed to 
## cacheSolve() which will return the inverse matrix for the specified 
## matrix wrapper object. The first call to cacheSolve() for a given 
## matrix wrapper object will calculate and cache the inverse matrix 
## of the original matrix (passed to makeCacheMatrix()). Subsequent 
## calls with that wrapper object will simply return the previously 
## cached inverse matrix.

####################################################################
## This function provides support for saving and retrieving a
## cached inverse matrix for the specified matrix argument.
## Effectively, it provides a wrapper class for the specified
## matrix argument that contains a local inverse matrix variable
## that can be set and retrieved using sub-functions.

makeCacheMatrix <- function(x = matrix()) {
        
        ## cached inverse of passed matrix x
        cachedinverse <- NULL
        
        ## Sets matrix variable in calling environment,
        ## and clears any currently cached matrix inverse.
        set <- function(y) {
                x <<- y
                cachedinverse <<- NULL
        }
        
        ## Returns the the current matrix variable 
        ## stored in the calling environment.
        get <- function() x
        
        ## Saves the passed matrix inverse into the current
        ## function environment's cachedinverse variable.
        setinverse <- function(inverse) cachedinverse <<- inverse
        
        ## Returns the cached matrix inverse stored in this
        ## functions environment.
        getinverse <- function() cachedinverse
        
        ## Returns the list of accessor functions supported 
        ## by this matrix inverse function object.
        list(
                set = set, 
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


####################################################################
## This function will return the inverse of the specified matrix
## wrapper object that was returned from a previous invocation of
## makeCacheMatrix() function. When this function is first called
## with a new matrix wrapper object, it will transparently pass 
## the original matrix along with the specified parameters to the
## solve() function to calculate the inverse matrix. This inverse 
## matrix is then stored in a local within the matrix wrapper object.
## Subsequent calls with the same matrix wrapper object will simply 
## return the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Use passed matrix wrapper object's function accessor 
        ## to check if the matrix inverse has already been 
        ## calculated and cached as a local variable in within
        ## the wrapper object.
        m <- x$getinverse()
        
        if(!is.null(m)) {
                ## The matrix inverse was cached, so 
                ## simply return the cached value.
                message("getting cached data")
                return(m)
        }
        
        ## Call wrapper object's get() accessor to
        ## retrieve the cached matrix.
        m <- x$get()
        
        ## Now calculate the inverse of the matrix.
        inverse <- solve(m, ...)
        
        ## Call the wrapper object's setter method 
        ## to cache the newly calculated inverse 
        ## as variable within the wrapper object.
        x$setinverse(inverse)
        
        ## Finally, return the inverse matrix.
        inverse
}
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	  ## This function creates a special "matrix" object that can cache its inverse.
  	  ##
  	  ## Args:
  	  ##   x: a square invertible matrix.
  	  ##
  	  ## Returns:
  	  ##   a special "matrix" object that can cache its inverse.        
        inverseValue = NULL
        set = function(original) {
                # use `<<-` to assign a value to an object in an environment different from the current environment. 
                x <<- original
                inverseValue <<- NULL
        }
        get = function() x

        setinverse = function(inverse) inverseValue <<- inverse 

        getinverse = function() inverseValue

        list(set=set, get=get, setinverse = setinverse , getinverse = getinverse)

}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	  ##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
        ##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
  	  ##
  	  ## Args:
  	  ##   x: output of makeCacheMatrix().
  	  ##
  	  ## Returns:
  	  ##  inverse of the original matrix input to makeCacheMatrix(). 
        
        inverseValue = x$getinverse()
        
        # I check if the inverse has already been calculated
        if (!is.null(inverseValue)){
                message("getting cached data")
                return(inverseValue)
        }
        
        # I calculate the inverse value if it hasn't been calculated 
        inverseValue = solve(x$get(), ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinverse(inverseValue)
        
        return(inverseValue)        
}

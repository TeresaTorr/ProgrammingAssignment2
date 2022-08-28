## This functions will give you the inverse of a special matrix and will hold
##the result in the cache. Then it will check the cache for the result.

## This first one creats the matrix and holds the result in the cache.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL 
        
        set <- function(y) {
                 x <<- y
                 inv <<- NULL
        }
  
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## This function computes and returns the matrix. If the matrix has not been 
## changed it will return the previous value.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
           message("Getting cached data")
           return(inv)
        }
        
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        inv
}




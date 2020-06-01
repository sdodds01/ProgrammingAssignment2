## These two functions allow the creation of a special matrix that can cache
## its inverse and the retrieval of that inverse.

## This function first gets the value of the matrix, then sets its value. It 
## then sets the inverse of the matrix and finally finds its value. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function calculates the matrix created with makeCachematrix. If the 
## inverse has been calculated already, then it retrieves the inverse from
## the cache. 

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setInverse(i)
        i
}

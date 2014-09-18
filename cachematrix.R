## Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
#repeatedly.

## The makeCacheMatrix function creates a special "matrix" object
## 1:   set the value of the Matrix
## 2:   get the value of the Matrix
## 3:   set the value of the Inverse of matrix
## 4:   get the value of the Inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) { ## set the value of the Matrix
                x <<- y
                i <<- NULL
        }
        get <- function() x ## get the value of the Matrix
        setinverse <- function(inverse) i <<- inverse ## set the value of the inverse of matrix
        getinverse <- function() i ## get the value of the inverse of matrix
        # Create a list containing the results of set, get, getinverse, getinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i) # Return the inverse of a matrix cached previously if 
                          # same matrix is run 
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i) # Cache the inverse value
        i ## Return a matrix that is the inverse of 'x'
        
}

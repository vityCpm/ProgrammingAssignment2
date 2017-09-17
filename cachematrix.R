## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). Your assignment is to write a pair of functions that 
## cache the inverse of a matrix.Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also 
## alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair 
## of functions that cache the inverse of a matrix.



## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) minv <<- inverse
        getinv <- function() minv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        minv <- x$getinv()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data)
        x$setinv(minv)
        minv
}




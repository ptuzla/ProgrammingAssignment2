## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. 
## These two functions, makeCacheMatrix() and cacheSolve() are used to cache the
## inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

# Sample output
# 
# > cacheMatrix = makeCacheMatrix(m1)
# > print(cacheSolve(cacheMatrix))
# [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
# > print(cacheSolve(cacheMatrix))
# getting cached data
# [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
# > print(cacheSolve(cacheMatrix))
# getting cached data
# [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
# >
# > cacheMatrix$set(m2)
# > print(cacheSolve(cacheMatrix))
# [,1] [,2]
# [1,]  0.3 -0.1
# [2,] -0.1  0.1
# > print(cacheSolve(cacheMatrix))
# getting cached data
# [,1] [,2]
# [1,]  0.3 -0.1
# [2,] -0.1  0.1
# > print(cacheSolve(cacheMatrix))
# getting cached data
# [,1] [,2]
# [1,]  0.3 -0.1
# [2,] -0.1  0.1

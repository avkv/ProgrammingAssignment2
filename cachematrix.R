## Assignment 2
## rprog-009
## 
## Please see at the end of file my session testing these functions
##
##
## In this file makeCacheMatrix and cacheSolve funtions are implemented
## These functions take advantage of R lexical scoping to provide a cached wrapper
## of the matrix inverse (solve) function.

## Given a matrix x return an "object" that has same data, but capable of caching
## its value and the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
  
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## The function below, expects a cachedMatrix object as a parameter and
## returns the inverse.  It uses the built in solve function.  But it will 
## compute inverse only if needed.  If the value of original input did not change
## and invere is previously calculated, it simple returns
## it display "getting cached data" message when the inverse from cache is
## being returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

#---------------- interactive session showing my testing ---------
# > m <- matrix(c(4,3,3,2),2,2)
# > solve(m)
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > source('~/Dropbox/coursera/rprog/ProgrammingAssignment2/cachematrix.R')
# > cachedm <- makeCacheMatrix(m)
# > cachedminv <- cacheSolve(cachedm)
# > cachedminv
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > cachedminv %*% cachedm$get()
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > cachedminv <- cacheSolve(cachedm)
# getting cached data
# > cachedminv
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > cachedminv %*% cachedm$get()
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > 

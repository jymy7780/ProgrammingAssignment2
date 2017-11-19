## R-Programming Week 3 - Peer Reviewed Assignment
## This set of functions are designed to evaluate
## the Lexical Scoping features within R.

## "makeCacheMatrix" loads a matrix into R as a
## cache

makeCacheMatrix <- function(x=matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## "cacheSolve" looks into the established cache
## to determine if the matrix inverse has been cached,
## if not, then calculate and print out the inverse

cacheSolve <- function(x, ...){
        m <- x$getinverse()
        if(!is.null(m)){
                message ("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

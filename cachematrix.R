## Calculatest the inverse of specialmatrix and cache the 
## value of the inverse so that when we need it again, it can be looked up 
## in the cache rather than recomputed.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        ## Returns the invesrs of matrix if it is already calculated
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##calculates the inverse for new matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}



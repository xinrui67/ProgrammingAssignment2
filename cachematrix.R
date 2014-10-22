## Below are two functions makeCacheMatrix() and cacheSolve() are used to
## create a special object that stores a square matrix and caches its inverse.

## The makeCacheMatrix() function creates a special square "matrix", 
## which is really a list containing a function to
## 1, set the value of the square matrix
## 2, get the value of the square matrix
## 3, set the value of the matrix inverse
## 4, get the value of the matrix inverse
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


## The cacheSolve() function calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix() function. It first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the mean of the data 
## and sets the value of the inverse in the cache.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
	  m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## This function creates a 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     x <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) m <<- inverse
     getInverse <- function() m
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the matrix from makeCacheMAtrix function
## or return cached value if it's already been calculated 35ddfdcbda8e294d673e751db6ac9c609d5505e4

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()
     if(!is.null(m)) {
          message('getting cached data')
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInverse(m)
     m
     
}

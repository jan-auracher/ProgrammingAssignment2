## The program comprises two functions:
## makeCacheMatrix takes a matrix and creates a matrix-object that has functions to 
## ... create the inverse and to store it in the cache
## cacheSolve creates the inverse of a "makeCacheMatrix"-matrix or - if there is one in the cache
## ... takes it from the Cache and returns the inverse

## Creates a special matirx with four functions: set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix(, nrow = 0, ncol = 0)
    set <- function(y) {
        x <<- y
        m <<- matrix(, nrow = 0, ncol = 0)
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Creates the inverse of this special matrix or takes it from the cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(ncol(m) > 0) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
    ## Returns a matrix that is the inverse of 'x'
}

## Test: Example with 2x2 matirx returns product of matrix with its inverse => identity matirx
#x = matrix(c(7,9,8,2), nrow = 2, ncol = 2)
#j <- makeCacheMatrix(x)
#inv <- cacheSolve(j)
#e <- x %*% inv
#e
#print("qed")

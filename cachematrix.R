## Makes a matrix-like object and takes the inverse of it. 
## If the solution to the inverse is in cach, it takes it from here. Otherwise it makes a new solution

## This function take a matrix and creates a matrix-like object. It wil cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
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


## This functino cheks if the inverse is in cach. If not it solves the matrix. 

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}

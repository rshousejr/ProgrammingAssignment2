## These two functions work together to calculate the inverse of a given matrix,
## assuming that the matrix provided is always invertible. 

## makeCacheMatrix will creates a vector consisting of a list containing a function to
## set and get the value of the vector, as well as set and get the value of the inverse
## of the matrix provided to the function as 'x'

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- inverse_of_matrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will take a value from the user as 'x', which is assumed to be an invertible matrix.
## it will then examine the value of i using the getinverse() function to see if data already exists in the cache.
## if data does exist in the cache, then that data is returned. Otherwise, it calls the setinverse function in 
## makeCacheMatrix to calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

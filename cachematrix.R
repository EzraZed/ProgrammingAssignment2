## The functions calculate the inverse of a matrix and store the solution


## This function takes the values of a matrix, sets the matrix, retrieves the matrix,
## sets the inverse of the matrix and retrieves the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, 
            getinverse = getinverse)
    
}


## This function returns the inverse of a matrix
## It returns the chached inverse if it already exists or
## calculates the inverse of the matrix if it does not yet exist

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

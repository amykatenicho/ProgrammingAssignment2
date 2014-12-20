## The functions below allow you to create a matrix and calculate the inverse of the given matrix
## Calculating the inverse of a matrix is computationally costly
## Therefore being able to retrieve a cache value rather than re-calculating each time is beneficial
## Call the makeCacheMatrix function with a new matrix
## Next call the cacheSolve function with the output of makeCacheMatrix to return the inverse of the matrix supplied
## If the matrix has already been inverted, the cached inverse will be returned

## makeCacheMatrix creates a matrix that can be cached 
## and contains variables that can be called such as get, set, getinverse and setinverse

makeCacheMatrix <- function(x = matrix()) {
        i <- matrix()
        set <- function(y) {
                x <<- y
                i <<- matrix()
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes an input of a matrix created via the previous function (makeCacheMatrix) and checks if getting the inverse variable already exists
## if the inverse already exists, the cached variable is returned, else the inverse is calculated

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!(all(is.na(i)))) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}

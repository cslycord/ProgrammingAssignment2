## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix makes a list of:
## 1. A function to set the value of the matrix
## 2. A function to get the value of the matrix
## 3. A function to set the inverse of the matrix
## 4. A function to get the inverse of the matrix
## Called by way of CacheMatrix <- makeCacheMatrix(matrix_name)
makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse_matrix <<- solve
        getinverse <- function() inverse_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This will calculate the inverse of the special matrix created by makeCacheMatrix
## It will first check if the inverse matrix has already been calculated via
## and use the cached value if it has been calculated.
## If the inverse has not been calculated, it will calculate it, set the cached
## inverse, and return the inverse
## Called by way of cacheSolve(CacheMatrix)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Here the function defined as makeCacheMatrix creates a special matrix that can cache its inverse. 
# This function sets value of the matrix and gets value of the matrix
# Also, it sets the inverse of the matrix and gets the inverse of the matrix
# Then it returns a list of of the functions

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) inverse <<- solve
        get_inverse <- function() inverse
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function

# The cacheSolve function calculates the inverse of the matrix with the makeCacheMatrix function.
# It checks if inverse is already calculated or not and returns inverse from cache if already been calculated.
# if not calculated it calculates the inverse using solve function which gives inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
                message("getting inverse of the matrix")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$set_inverse(inverse)
        inverse
}



# m1 <- matrix(c(1, 5, 7, 10), 2, 2)
# result1 <- makeCacheMatrix(m1)
# print(result1)
# print(result1$get())
# print(result1$get_inverse())

# result2 <- cacheSolve(result1)
# print(result2)
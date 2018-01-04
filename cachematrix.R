## makeCacheMatrix and cacheSolve are two paired functions that are used to 
## create a special object that stores a matrix and cache's its inverse.

## the makeCacheMatrix creates a special "matrix" ,that is actually a list 
## containing functions to set the matrix,get the matrix, calculate the inverse
## of the matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    im <-NULL
    set <- function(y){
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setim <- function(inverse) im <<- inverse
    getim <- function() im
    list(set = set,get =get,
         setim = setim,
         getim = getim)

}


## the cacheSolve function calculates the inverse of the special "matrix" created
## with the makeCacheMatrix function. It first checks if the inverse of the
## matrix has already has beeb calculated. If so, it gets the inverse from the
## cache and skip the computation. Otherwise, it calculates the inverse of the
## matrix and sets the values of the inverse in the cache through the setim 
## function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getim()
    if(!is.null(im)){
        message("getting cached inverse of the matrix")
        return(im)
    }
    matr <- x$get()
    im <- solve(matr)
    x$setim(im)
    im
}

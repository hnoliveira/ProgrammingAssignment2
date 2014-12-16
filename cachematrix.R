## These two functions (makeCacheMatrix and cacheSolve) are designed to inverte
## a square matrix (provide it is inversible) avoiding to replicate the work if
## the matrix has been alreary inverted. 
##   
##  
## This function will get as an entry the matrix to be inverted and generate
## a list of three functions that will be used in the next function (cacheSolve)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    getMatrix <- function() x
    setsol <- function(sol) inv <<- sol
    getsol <- function() inv
    list( getMatrix = getMatrix,
          setsol = setsol,
          getsol = getsol)
}


## The argument of this function is the list of functions that was generated 
## in (makeCacheMatrix). First it verify if the inverse was already computed. 
## if so it returns the inverse, else it wil call de solve function to invert
## the matrix
cacheSolve <- function(x, ...) {
    inverse <- x$getsol()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    Matrix <- x$getMatrix()
    inverse <- solve(Matrix, ...)
    x$setsol(inverse)
    inverse  
}


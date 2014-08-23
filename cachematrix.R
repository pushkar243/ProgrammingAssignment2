# makeCachematrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the
# matrix. Contains the following functions:
# * setMatrix set the value of a matrix
# * getMatrix get the value of a matrix
# * cacheInverse get the cached value (inverse of the matrix)
# * getInverse get the cached value (inverse of the matrix)
#


#makeCacheMatrix <- function(x = matrix()) {

#}

makeCacheMatrix <- function(x = matrix()) {
set <- function(M) {
A <<- M
AINV <<- NULL
}
if(exists("A")) {
if(all(dim(x) == dim(A)) && all(apply(x == A, c(1,2), all))) {
###this means matrix is already in cache, no need to change anything
message("INFO: Matrix is in cache, nothing changed")
} else {
message("INFO: New matrix given, reseting cache")
set(x)
}
} else {
set(x)
}	
get <- function() A
setinv <- function(MI) AINV <<- MI
getinv <- function() AINV
list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function

## cacheSolve takes an object returned by makeCacheMatrix and
## either it retrieves the inverse from cache, or computes the inverse
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of that set in 'x'
MI <- x$getinv()
if(!is.null(MI)) {
message("INFO: Inversed matrix retrieved from cache")
return(MI)
}
M <- x$get()
MI <- solve(M, ...)
x$setinv(MI)
MI
}



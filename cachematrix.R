## This functions are suppose to make a special Matrix
## and to solve for its inverse or get the inverse if previously calculated it

## This function manage the creation of the special matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invM) inv <<-invM
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv =getinv)
}


## This function get the inverse of the matrix if previously 
##calculated it and calculate the inverse otherwise

cacheSolve <- function(x, ...) {
        inv <-x$getinv()
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        Mat <- x$get()
        inv <- solve(Mat, ...)
        x$setinv(inv)
        inv
}

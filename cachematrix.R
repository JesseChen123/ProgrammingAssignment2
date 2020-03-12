## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        # set the initial value
        inv <- NULL
        
        # set value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get value of the matrix
        get <- function() x
        
        # set value of the inverse
        setinv <- function(inverse) {
                inv <<- inverse
        }
        
        # get value of the inverse
        getinv <- function() inv
        
        # return the list of functions
        list(set=set, get=get, setinv=setinv, getinv=getinv)
                
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}

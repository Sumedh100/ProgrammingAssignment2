## This program has 2 functions makeCacheMatrix & cacheSolve that cache the inverse of a Matrix(invertible)

## makeCasgeMatrix creates a matrix object that caches its inverse for the input matrix which is an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function(){
                x
        }
        
        setinv <- function(inverse){
                inv <<- inverse
        }
        
        getinv <- function(){
                inv
        }
        
        list(
                set = set,
                get = get,
                setinv = setinv,
                getinv = getinv
        )
        
}


## cacheSolve computes the inverse of the input matrix
## However it first checks whether the inverse has already been calculated and cached
## if Yes then it just retrieves the result from the cache and returns it

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        if(!is.null(inv)){
                message("getting cached matrix")
                return(inv)
        }
        
        inputMatrix <- x$get()
        inv <- solve(inputMatrix, ...)
        
        x$setinv(inv)
        
        inv
}

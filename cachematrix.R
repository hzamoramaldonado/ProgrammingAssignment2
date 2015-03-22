#The first function, makeCacheMatrix creates a special "matrix", 
#which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <<- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <<- function() x
        setinv <<- function(solve) inv <<- solve
        getinv <<- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


#This function does the following:
# calculates the inverse of the special "matrix" created with makeCacheMatrix
# first checks to see if the inverse has already been calculated.
#if yes, it skips the calculation 
#if not it calculates the inverse and puts it in the cache

cacheSolve <- function(x, ...) {
       inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv

}

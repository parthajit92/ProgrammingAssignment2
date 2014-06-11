#makes cacheMatrix object, which has function for setting values to the matrix, getiing the vlaue, calculating inverse of the matrix
#value of 'm' define the state of the matrix, if it is new or modified using set function then m=NULL

makeCacheMatrix <- function(x = matrix()) {
        #when a new cacheMatrix is created m would be set to NULL.
        m <- NULL
        set <- function(y) {
                #assign input matrix to x, and m to null.
                #Whenever the matrix changes m would be set to null
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}
# cacheSolve takes a CacheMatrix, then it checks the 'm' value of the cacheMarix, if it is null it calculates the inverse, if not
# it returns the already calculated value of inverse which was calculated during earlier call to cacheSolve function.
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                #old matrix and matrix value not changed by set command.
                # no need to recalculate just return m
                message("getting cached data")
                return(m)
        }
        # will reach here only if matrix is new or changed.
        # get matrix
        data <- x$get()
        #calculate the inverse
        m <- solve(data, ...)
        # store this inverse for future use.
        x$setsolve(m)
        #return the inverse.
        m
}

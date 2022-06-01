

makeCacheMatrix <- function(x = matrix()) {
		invr <- NULL
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) invr <<- inverse
        get_inverse <- function() invr
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


##

cacheSolve <- function(x, ...) {
        invr <- x$get_inverse()
        if(!is.null(invr)) {
                message("getting cached mtrx")
                return(invr)
        }
        mtrx <- x$get()
        invr <- solve(mtrx, ...)
        x$setmean(invr)
        invr
}

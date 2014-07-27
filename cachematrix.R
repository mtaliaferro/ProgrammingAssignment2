## Functions that cache the inverse of a matrix

makeCacheMatrix <- function( m = matrix() ) {

    i <- NULL
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Get matrix
    get <- function() {
    	m
    }

    ## Set and get the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    getInverse <- function() {
        i
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute inverse of matrix.
cacheSolve <- function(x, ...) {

    m <- x$getInverse()

    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    data <- x$get()

    ## Calculate the inverse
    m <- solve(data) %*% data

    x$setInverse(m)

    m
}

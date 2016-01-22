## The functions below generate a list capable of caching a matrix's
## inverse and pull that cached value (or find and cache the inverse,
## if no value has been cached yet)

## makeCacheMatrix creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y){
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) im <<- solve
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve returns a matrix that is the inverse of x.
## First checks to see if the value is already cached,
## then either displays the cached value or finds the inverse,
## caches the value, and prints the value for the user.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        ## Try to return cached inverse
        if (!is.null(im)){
                print(c("Value of !is.null(im)",!is.null(im)))
                message("getting cached data")
                return(im)
        }
        ## If none exists, use solve function and cache this
        ## new value in 'x'
        data <- x$get()
        im <- solve(data)
        x$setinverse(im)
        im
}

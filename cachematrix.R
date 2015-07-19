## Put comments here that give an overall description of what your
## functions do

## For a matrix "x", creates a "CacheMatrix" that can store 
## the matrix x and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL 
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)

}


## for a CacheMatrix "x" as above, checks to see if the inverse
## is has been calculated. If so, it returns the cached value,
## otherwise it calculates the inverse and caches it in x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("Getting cached data")
		return(inverse) }
	data <- x$get()
	inverse <- solve(data)
	x$setinverse(inverse)
	inverse	
}

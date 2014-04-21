## The following functions create a unique matrix and then set up a cache
## system for the matrix and its inverse over two different functions.

## 1. makeCacheMatrix creates the matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) { 
		##creatematrix function sets parent matrix to y
		x <<- y
		m <<- NULL
	}
	get <- function () x ## returns value of matrix to check
	setinverse <- function(inverse) {
		m <<- inverse ## sets inverse
	}
	getinverse <- function() m 
		## returns inverse (null returned if none exists)
	
	list(set = set, get = get, setinverse = setinverse, getinverse =getinverse)
}


## 2. cacheSolve computes the inverse of the matrix created by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse() 
	## checks to see if an inverse is already there
	y <- x$get() ## checks matrix that is already in parent environment
	if (!is.null(m) && y == x$get()) { 
		## checks that both the inverse is cached and that a matrix exists
		message ("getting cached data")
		return (m)
	}
	data <- x$get()
	m <-solve(y)
	x$setinverse(m)
	m
}

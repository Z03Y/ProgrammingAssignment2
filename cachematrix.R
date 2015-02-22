## caching the inverse of a matrix 

## constructor function that creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	## initialize the inverse of a matrix i to NULL
	i <- NULL

	## set (re)initializes x and i; get returns x 
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x

	## setinverse assigns a calculated inverse to i; getinverse returns i
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i

	## a list containing all the functions inside makeCacheMatrix and assigned
	## to their name, so they can be accessed by name
	list(set = set, get = get, setinverse = setinverse, 
				getinverse = getinverse)
}


## function to calculate the inverse of the matrix object returned by the 
## function above makeCacheMatrix if it has not been calculated yet; if it has
## been calculated, it retrieves its inverse from the cache

cacheSolve <- function(x, ...) {
      ## returns a matrix that is the inverse of 'x'

	## gets the value stored in i and checks if it is NULL; if it is, skip
	## the conditional to calculate the inverse; if it is not, return i which
	## is the calculated and cached inverse of x
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	## get the matrix x, calculate its inverse, call setinverse and return the
	## calculated inverse
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i	
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function used to make a CacheMatrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL    ## initially initialize the inverse as null
	set <- function(Y) {
		x<<-y
		inv <<-NULL
	}
	get <- function() {x}
	setInverse <- function(inverse) {inv <<- inverse}
	getInverse <- function() {inv}
	list(set =set , get = get , setInverse = setInverse , getInverse = getInverse)
}


## Write a short comment describing this function
## function to solve the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse() 
	if(!is.null(inv)){
		message("getting cached data")  ## displays if cache data is done
		return(inv)   ## returning inverse matrix
	}
	mat <- x$get()
	inv <- solve(mat,...)
	x$setInverse(inv)
	inv
}

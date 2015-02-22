# In instances where you have a very long vector or array and
# it needs to be evaluated to complete a simple computation, 
# you may want to figure out a way to decrease the evaluation and
# computation time when using that simple computation in the
# future. If the contents of the vector/array are not changing, it
# is possible to write a function that will cache the value of the
# computation. That way, the function will just look up the value,
# rather than going through the time-consuming computation all
# over again. The computation we will be doing is matrix
# inversion, which usually takes quite a while. To cut down on
# time, we will create 2 functions, createCacheMatrix and
# cacheSolve.

# In the createCacheMatrix function, we will use the <<- operator.
# This operator will enable us to assign a value to an object in
# an environment that is different than the current environment
# we are working in.
#
# The createCacheMatrix function will create a list that contains
# the functions to first set the value of the matrix, then get the
# value of the matrix, set the value of the inverse, and lastly,
# get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
 	list(set = set, get = get,
 		setinverse = setinverse,
 		getinverse = getinverse)
}


# The function cacheSolve will calculate the inverse of the list
# created in makeCacheMatrix, however, it will check to see if
# the inverse has already been calculated. If it has, then it will
# get the inverse from the cache and not perform the computation.
# If it sees that the inverse has not been calculated, then it
# will perform the calculation via the setinverse function.


cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}

## Here is a sample of our new functions in action:
## > x = rbind(c(2, 7), c(7, 2))
## > z = makeCacheMatrix(x)
## > z$get()
##      [,1] [,2]
## [1,]    2    7
## [2,]    7    2

## First run, no cache:
## > cacheSolve(z)
##             [,1]        [,2]
## [1,] -0.04444444  0.15555556
## [2,]  0.15555556 -0.04444444

## Second run, will retrieve from cache:
## > cacheSolve(z)
## getting cached data
            [,1]        [,2]
## [1,] -0.04444444  0.15555556
## [2,]  0.15555556 -0.04444444
## > 


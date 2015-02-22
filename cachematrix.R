##  A pair of functions that cache the inverse of a matrix
##  Plus a function to test the code

## Creates and returns the required set of functions to cache a matrix and its inverse
## The key to this assignment is 
## 1.  The cached matrix is stored in the "special" variable using the super-assignment operator
## 2.  The inverse of the matrix is stored in the "inverse" variable using the super-assignment operator
## 3.  Inverse is set to NULL initially and when the special matrix changes.  This avoids the need
##     to check that the matrix hasn't changed

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y)
	{
		special <<- y
		inverse <<- NULL
	}
	get <- function() special
	setInverse <- function(i) inverse <<- i
	getInverse <- function () inverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calulates the inverse if it hasn't already been calculated and stores it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	if (!is.null(inverse))
	{
		## message("getting cached data")
		return (inverse)
	}
	data <- x$get()
	inverse <- solve(data)
	## message("calculated inverse to be ", inverse)
	x$setInverse(inverse)
	inverse
}

## The following is some test code to ensure this code works correctly
## The correct inverse of the matrix in the first line is  
## 0.04878049   0.17073171
## -0.1219511   0.07317073
## Special thanks to the free on-line Khan Academy for reminding me how matrix inversion works 

test <- function()
{
	m <- c(3, 5, -7, 2)
	dim(m) <- c(2, 2)
	r <- makeCacheMatrix(m)
	r$set(m)
	cacheSolve(r)
	x <- r$getInverse()
	print (x)
}
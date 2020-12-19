## Assignment: Caching the Inverse of a Matrix

## A closure in R is an object that contains functions that are bound to the 
## environment in which the closure was created. These functions maintain 
## access to the scope in which they were defined, they receive their name 
## because they enclose the environment of the main function and can access 
## all its variables.

## It also allows two levels of parameters: a main level that controls the 
## operation and a secondary level that does the work.

## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. 

## Given a square matrix A, if there is another matrix B of the same 
## order that verifies: A. B = B. A = I (I = identity matrix), B is 
## said to be the inverse matrix of A and is represented by A^-1.

## If the inverse matrix of A exists, the matrix A is said to be 
## invertible or regular. Otherwise, matrix A is said to be singular.

## an example of CLOUSURE in r, is given by the makeCacheMatrix() function, 
## which stores x and inv in the surrounding environment of the set, get, 
## setInverse, getInverse functions. That means the environment within which 
## they were defined, i.e. the environment created by makeCacheMatrix().

## makeCacheMatrix(): This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL 
	set <- function(matrix) { 
	x <<- matrix
	inv <<- NULL
    }
	get <- function() x 
	setInverse <- function(inverse) inv <<- inverse 
	getInverse <- function() inv 
	list(set = set, get = get,
	setInverse = setInverse,
	getInverse = getInverse)
}


## cacheSolve(): computes the inverse of the special "array" returned by 
## 'makeCacheMatrix ()'. If the inverse has already been computed (and the 
## array has not changed), then 'cacheSolve()' should retrieve the inverse 
## from the cache. The calculation of the inverse of a square matrix can be 
## done with the function 'solver' in R. For example, if X is an invertible 
## square matrix, 'solver(X)' will return its inverse. 

cacheSolve <- function(x, ...) {
	# Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if(!is.null(inv)) {
	message("getting cached data")
	return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}

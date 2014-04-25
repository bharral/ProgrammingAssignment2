##
## Routines to store a matrix with its inverse,
## thus avoiding recomputation
##
## Brian Harral -- 18 Apr 2014
##

## makeCacheMatrix: create the object that can store the matrix with its inverse
##		input: (matrix) x
##		output: a list of four functions
##			x$set(matrix) -- assigns "matrix" to x
##			x$get() -- returns the matrix stored in x
##			x$setinv(matrix) -- assigns "matrix" to be the cached inverse
##			x$getinv() -- returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

	inv<-NULL			## initialize the inverse matrix to NULL

	set<-function(y) {	## assigns input matrix to the data structure
		x<<-y
		inv<<-NULL
	}

	get<-function() x	## retrieve the matrix from the data structure

	setinv<-function(inverse) inv<<-inverse
						## assign argument to the "inverse slot" in the data structure

	getinv<-function() inv
						## retrieve the inverse matrix from the data structure

	list(set=set, get=get, setinv=setinv, getinv=getinv)
						## return a list containing these functions
}


## cacheSolve: uses the matrix object to invert the input matrix
##		input: matrix object created with call to makeCacheMatrix
##		output: normal matrix contaning the inverse of the matrix stored in the
##			matrix object

cacheSolve <- function(x, ...) {

## If matrix object already contains the inverse, return it

	invrs<-x$getinv()		## Retrieve inverse matrix if it has already been stored
	if (!is.null(invrs)) {
		return(invrs)		## Return the inverse matrix
	}

## Otherwise, compute the inverse using the solve function
##		and store it in the matrix object for next call

	data<-x$get()			## Retrieve the matrix
	invrs<-solve(data)		## Find its inverse
	x$setinv(invrs)			## Store inverse in data structure
	invrs					## Return the inverse matrix
}

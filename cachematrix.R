## Below are two functions that are used to create a special object that stores a matrix and cache’s its inverse.
## The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
## 	set the value of the matrix
## 	get the value of the matrix
## 	set the value of the inverse matrix
## 	get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {	## x is a matrix function
  inv <- NULL
  set <- function(y) {	       			## 1) original matrix for use to make inverse matrix
    x <<- y 		                	## <<- assign y to x
    inv <<- NULL			        ## assign inv to NULL before computing
  }
  get <- function() x       		        ## 2) set original matrix to get
  setMatrix <- function(solve) inv <<- solve	## 3) set the inverse of the matrix to setMatrix
  getMatrix <- function() inv                  	## 4) set the inverse of the matrix to getMatrix; 
                                                ##    and the result of this funtction as it is the last element
  list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}

## The following function calculates the inverse of the special “Matrix” created with the above function. 
## However, it first checks to see if the inverse matrix [solve()] has already been calculated. 
## If so, it gets the inverse matrix [solve()] from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix [solve()] of the data and sets the value of the inverse matrix in the cache via the setMatrix function.

cacheSolve <- function(x , ...) {
  inv <- x$getMatrix ()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrixdata <- x$get()
  inv <- solve(matrixdata, ...)
  x$setMatrix(inv)   	## or use return(inv)
  inv
}
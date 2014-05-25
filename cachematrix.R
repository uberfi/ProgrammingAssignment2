##########################################################
### Cos Fi
### R_Assignment2
### May 25, 2014
##########################################################

### The following two functions provide an example caching in R
### makeCacheMatrix: creates a matrix object that can cache its inverse, and 
### cacheSolve: retrieves Inverse from cache or computes the inverse of the 
### special matrix returned in makeCacheMatrix.


### The function makeCacheMatrix sets the value of the matrix;
### gets the value of the matric;
### sets the value of the Inverse; and 
### gets the value of the Inverse 


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	### Create a list of the functions: set, get, setInverse and getInverse
	list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


### The function cacheSolve accomplishes four things:
### it checks to see if the inverse of the matrix has already been calculated by makeCacheMatrix;
### if the Inverse exists, then it gets the Inverse via getInverse, skips any computation of inverse;
### if Inverse does not exist, then it computes the Inverse; and
### sets the Inverse in the cache via the setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse() ### Get the inverse if it exists
        if(!is.null(m)){
        	### If m is not empty, then get the Inverse from cache
        	message("getting cached Inverse")
        	return(m) ### Returns the Inverse
        }
        ### If there is no Invrse in the cache
        ### compute the Inverse and set the value of the Inverse in the cache
        data <- x$get()
        m <- solve(data) ### Here we are assumeing a square invertible matrix, as per assignment2
        x$setInverse(m)
        m
}

#####################################################################################
### Reference
### Attribution: James A. Stephenson
### https://class.coursera.org/rprog-003/forum/thread?thread_id=664
### The refrence to Stephenson's post and related discussion thread 
### is mighty useful in undersanding how to run the code
#####################################################################################

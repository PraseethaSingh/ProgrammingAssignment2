#  This file contains functions to cache the inverse of a matrix
##########################################################################
# Function  :  makeCacheMatrix()
# This function creates a special "matrix" object that can cache its inverse
#
# Arguments:
## x - a matrix 
#
# Return value: Returns a list containing the following elements
#               element 1 : a function set() which sets the input matrix  
#               element 2 : a function get() which fetches the inputs matrix  
#               element 3 : a function setinverse() which sets the inverse matrix               
#               element 4 : a function getinverse() which gets the the cached        #                           inverse matrix 
#                           
##########################################################################
makeCacheMatrix <- function(x = matrix()) {
        # initialise inverse to an empty matrix
        inverse = matrix()
        
        # set function sets the input matrix and inititalises the inverse to empty 
        set <- function(y) {
                    x <<- y 
                    inverse <<-matrix()
        }
        
        # get function returns the input matrix
        get <- function() {x}
  
        # setinverse function sets the inverse matrix
        setinverse<-function(z) { inverse <<- z}
        
        # getinverse function gets the inverse matrix 
        getinverse<- function() {inverse} 
  
        
        # returns list with these function names as elements
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
  
}

##########################################################################
# Function  :  cacheSolve()
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.
#
# Arguments:
## x   - input matrix
## ... - other arguments
#
# Return value: Returns the inverse of the input matrix 
#                           
##########################################################################
cacheSolve <- function(x, ...) {
        ## m gets the value of the inverse matrix
        inverse_mat <- x$getinverse()
        
        ## check if the inverse matrix is empty 
        if(!is.na(inverse_mat)) {
            # if the inverse matrix is not an empty matrix, 
            # return the inverse from the cache
            message("getting cached data")
            return(inverse_mat)
        }
        # if the inverse matrix is an empty matrix, the calculate the inverse of x
        
        ## fetch the input matrix 
        input <- x$get()
        
        ## compute matrix inverse
        inverse_mat <- solve(input)
        
        ## set the value of inverse matrix 
        x$setinverse(inverse_mat)
        
        ## Return a matrix that is the inverse of 'x'
        inverse_mat
        
}

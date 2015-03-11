## The following function creates a list to cache the inverse of a matrix  

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  
#initialises object i to NULL in the defining environment.
    set <- function(y) {#sets the value of the matrix
        x <<- y     
# sets  variable x to the value of inputed matrix in the calling environment.
        i <<- NULL 
#initialises variable i to NULL in the calling environment.
    }
    get <- function() x  
#gets the value of the matrix
    setinverse <- function(inverse) i <<- inverse  #sets the value of the inverse to i in the calling environment.
    getinverse <- function() i 
#gets the value of the inverse matrix from the defining environment.
    list(set = set, get = get,  
#lists the defining steps/functions
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function checks for a cached matrix inverse and if one does not exist it creates one. 
cacheSolve <- function(x, ...) {
    i <- x$getinverse() 
#the value the inverse (from the defining environment) is assigned to i
    if(!is.null(i)) { 
#checks for cached inverse and if found 
        message("getting cached data") 
#returns message and cached inverse.
        return(i)
    }
    data <- x$get() 
# value of the matrix (from defining environment) is assigned to data
    i <- solve(data, ...) 
#value of inverse is calculated and assigned to i
    x$setinverse(i) 
#value of the inverse is set as cached
    i                     
#inverse is returned
}


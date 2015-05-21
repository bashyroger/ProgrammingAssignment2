## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    
    ##function to cache matrix and inverse in different environment
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    
    ## Functions to get input maxtix, set and get the inverse matrix
    get <- function() x
    setinverse <- function(inverse) invm <<- inverse
    getinverse <- function() invm
    
    ##return as list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) { 
    
            ##check whether inverse is already calculated, if so, get from cache
            invm <- x$getinverse()
            if(!is.null(invm)) {
                message("getting data from the cache...")
                return(invm)
            }
            
            ##get input matrix
            data <- x$get()
            
            ##not needed for assignment, but check whether matrix is square or not!
            if(nrow(data)==ncol(data)) {         
                ##If square, then calculate inverse
                invm <- solve(data, ...)            
                x$setinverse(invm)
                return(invm)
            
            } else {
                #If NOT square, return message and NA
                message("Matrix is not square, Cannot calculate inverse")
                return(NA)
                
            } 

}

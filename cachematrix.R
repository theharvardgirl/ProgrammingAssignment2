#This function creates a special "matrix" object that can cache its inverse
#This function is divided into 4 sub functions: setmatrix, getmatrix, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL           
        setmatrix <- function(m) {         #sets value of the matrix into the cache
                x <<- m
                inverse <<- NULL           #when a new matrix is input, value of "inverse" is set to null in the cache
        }
        
        getmatrix <- function() x          #gets value of the matrix from the cache
        
        setinverse <- function(inv)        #sets value of the matrix inverse into the cache
                #check that "inverse" already stored in cache is not null 
                #and is the correct inverse for matrix via multiplication
                if (!is.null(inverse) && identical(round(x%*%inverse), diag(max(dim(x))))){
                        return(inverse)
                } else {                           #if no inverse in cache, check that the inverse user inputs
                        #is the correct inverse for matrix via multiplication
                        if (identical(round(x%*%inv), diag(max(dim(x))))) {
                                inverse <<- inv
                        }
                        else {                     #if no inverse in cache and the user-entered inverse is 
                                #incorrect, set the value of inverse to null in cache
                                inverse <<- NULL      
                        }
                }
        
        getinverse <- function() inverse   #gets value of the matrix inverse from cache
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()       #retreives inverse of matrix x stored in the cache
        if(!is.null(inverse)) {         #if inverse is not null, returns value of inverse in cache
                message("getting cached data")
                return(inverse)
        }                               #else, calculates the value of inverse via funcition solve(x)
        m <- x$getmatrix()
        inverse <- solve(m, ...)
        x$setinverse(inverse)           #and sets inverse to the calculated value
        return(inverse)
}
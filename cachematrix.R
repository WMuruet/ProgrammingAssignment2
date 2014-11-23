## these functions create a special matrix object that can be cached. 
## it the computes the inverse matrix
## but they will only compute the inverse matrix 
## if it is not already in the cache. 
## If the inverse matrix is already in the cache, 
## cacheSolve will just retrive it

## This function creates a special "matrix" object. 
## The inverse of this object can be "cached"
## it returns a list that will be used as input by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        
	  inversematrix = NULL
      
	  setmatrix = function(y) {
        x <<- y
        inversematrix <<- NULL
        }
        getmatrix = function() x
        setinversematrix = function(inverse) inversematrix <<- inverse 
        getinversematrix = function() inversematrix
        list(set=setmatrix, get=getmatrix, setinv=setinversematrix, getinv=getinversematrix)
}
##a list containing functions to (1) set the matrix and (2) get the matrix 
## aswell as to (3)set the inverse and (4)get the inverse was returned
## this list will be used as input by cacheSolve()

## This function returns the inverse matrix. 
## it computes the inverse matrix if it has not already been calculated. 
## Otherwise, it retrives the inverse from the cache

cacheSolve <- function(x, ...) {
            
        inversematrix = x$getinv()
        
## If the inverse has already been calculated then...
        if (!is.null(inversematrix)){
             print("Retriving Cache...please wait")
             message(inversematrix)   
             return(inversematrix)
               
                
        }
## If the inverse has NOT already been calculated...
        else { 
        mat.data = x$get()
        inversematrix = solve(mat.data, ...)
        x$setinv(inversematrix)
        print("Computing...please wait")
        message(inversematrix)
        return(inversematrix)
         }
}


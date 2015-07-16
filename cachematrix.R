## These two functions create a cached square "matrix" object (makeCacheMatrix) and computes then caches
## its inverse for faster retrieval (cacheSolve).

## makeCacheMatrix: This function creates a cached square "matrix" object with a set(), get(), setSolve(),
## and getSolve() internal functions. Those internal functions can be accessed using the name you gave
## the matrix with $ and the name of the function, e.g. mat$get(). 

makeCacheMatrix <- function(x = matrix()) {

      ## Nulling any values of "solve" variable when creating a new matrix
      s <- NULL
      
      # To create the matrix with values that are passed with the function
      rc = sqrt(length(x))
      x = matrix(x, rc, rc)
      
      set <- function(y) {
            
            ## Determining number of rows & columns in the matrix
            rc = sqrt(length(y))
            
            ## Testing ability to create a square matrix
            if ((rc %% 1) == 0) {
            
            ## Creating the square matrix and caching it
            x <<- matrix(data=y, nrow=rc, ncol=rc)
            s <<- NULL
            
            # Returning an ERROR message with ADVICE if square matrix cannot be created
            } else {
                  addElements = ((as.integer(rc) + 1)^2) - length(y)
                  subtractElements = length(y) - ((as.integer(rc))^2)
                  message("Square matrix cannot be created with ", length(y), " elements. Please add ",
                        addElements, " or subtract ", subtractElements, 
                        " elements from this matrix or provide data that can be resolved as a square matrix.")
                  
            }
      }
      
      get <- function() x
      setSolve <- function(sol) s <<- solve(x)
      getSolve <- function() s
      list(set = set, get = get,
           setSolve = setSolve,
           getSolve = getSolve)

}

## cacheSolve: This function retrieves or computes the inverse of the matrix returned by makeCacheMatrix
## above. The inverse is retrieved from cache if it has already been calculated for an unchanged matrix;
## otherwise, if the matrix has changed or has not yet been calculated, the function will first compute
## the inverse and set it to cache. Calling the function can be done with cacheSolve(matrixName).
## matrixName is the name you called the matrix when using makeCacheMatrix.

cacheSolve <- function(x) {

      s <- x$getSolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data)
      x$setSolve(s)
      s
}

## The first function caches a matrix that is assumed to be invertible.
## The second function finds either the inverse of a cached matrix or
## solves for the inverse of a new one.  Both were constructed by analogy
## from the example provided by our mentor Len.

## makeCacheMatrix will cache a matrix and make it available to the second function.
## Line 16 initializes iv with NULL.  Lines 17-20 are the 'setter' for the matrix.
## The 'setter' is assigning values to objects in the parent environment(x, iv).
## Line 21 contains the 'getter' for the matrix.
## Lines 22-24 contain the 'setter' for the inverse. Value can be found in parent env.
## Line 25 contains the 'getter' for the inverse.  cacheSolve will retrieve
## a stored inverse using this function.  Line 26 stores all the functions and makes
## them available outside the function by naming them.  makeCacheMatrix becomes an object.

makeCacheMatrix <- function(x = matrix()){
    iv <- NULL
    setmatrix <- function(y){
      x <<- y
      iv <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inverse){
      iv <<- inverse
    }
    getinverse <- function() iv
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}    


## cacheSolve checks to see if there is a cached inverse matrix.  If not, it solves
## for the inverse of the input--a matrix of the form makeCacheMatrix().
## Line 40-44 check to see if there is a cached inverse matrix. 
## If so--if the value is not NULL--a message is printed and value is returned. 
## Line 45 gets a new input matrix if there is not one cached.  
## Line 46 finds the inverse of the new input matrix.
## Line 47 sets the value of the new inverse so that it can be cached next time.
## Line 48 displays the inverse

cacheSolve <- function(x, ...){
  iv <- x$getinverse()
  if(!is.null(iv)) {
      message("Getting cached data.")
      return(iv)
  }
  dat <- x$getmatrix()
  iv <- solve(dat, ...)
  x$setinverse(iv)
  iv
}

  



## makeCacheMatrix converts a matrix into a list of 4 functions and stores the matrix  
## cacheSolve takes the results of the first function and either calculates the inverse or recalls a previously
## calculated inverse from a cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL ## m is set to NULL to reset the value of m if this function has been previously called
      set <- function(y) {
            x <<- y
            m <<- NULL ## if the matrix changes the cached inverse will need recalculating, so m is set to NULL
      }
      get <- function () x ## this function stores the original matrix
      setinverse <- function(solve) m <<- solve ## this allows to set the inverse
      getinverse <- function() m ## this gets the inverse from the cache
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse) ## makes the list

}


## This function uses the result of the makeCacheMatrix function to either calculate the inverse of the matrix,
## or print the cached inverse matrix

cacheSolve <- function(x, ...) { 
      m <- x$getinverse() ## looks in cache to get inverse
      if(!is.null(m)) { ## checking to see if cache is not empty
                  message("getting cached data")
                  return(m) ## prints inverse and exits the function
      }
      data <- x$get() ## sets data to original matrix stores in makeCacheMatrix
      m <- solve(data, ...) ## calculates inverse
      x$setinverse(m) ## sets inverse in cache
      m ## prints inverse
}

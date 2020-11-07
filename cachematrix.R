## As per course example, The goal is to set a matrix using 'set' and extract 
##it using 'get, setinvMatrix would calculate inverse based on x and then getinvMatrix
## would release this.

##  Finally, list at the end to allow accessing via $.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
      x <<- y
      m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## In this function, inv objective is used to get the matrix and if cached,
## the function is designed to return cached data and if not ,it will calculate
## as well as set and then return a calculated inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      if(!is.null(m)) {
      message("getting cached data")
      return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}

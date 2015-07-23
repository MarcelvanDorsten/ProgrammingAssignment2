## This function takes advantage of the scoping environment and reads from this 
## cache and does his computation therefore faster especially by big files  


## Invert the variables and put them in the scoping environment

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
          x <<- y
          m <<- NULL
      }
      get <- function() x
      setmatrix <- function(matrix) m <<- matrix
      getmatrix <- function() m
      list(set = set, get = get, 
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}

## Calculates the matrix of the above created makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}

## makeCacheMatrix creates the inverse of a matrix and caches it in m


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve<- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}

## cacheSolve reads out m, and if not empty uses the result. Otherwise it calculates the inverse of a matrix

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


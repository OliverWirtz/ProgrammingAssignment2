## makeCacheMatrix creates the inverse of a matrix and caches it in m


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL       ##initialise m 
    set <- function(y) {  
      x <<- y         
      m <<- NULL
    }                     ##define a set function that can assign an object to x and resets m  
    get <- function() x   ## the get function retrieves x
    setsolve <- function(solve) m <<- solve      ##setsolve uses the solve function on the parameter and assigns result to m
    getsolve<- function() m     ##retrieve m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)      ###create a list with function calls created above

}

## cacheSolve reads out m, and if not empty uses the result. Otherwise it calculates the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()              ##call the getsolve function which retrieves m
  if(!is.null(m)) {
    message("getting cached data")  ##if m is not null the this result is used
    return(m)                       ### return m and quit function
  }
  data <- x$get()                  ### in case m is null call function get, basically assign x to data
  m <- solve(data, ...)            ## calculate inverse from data and assign result to m
  x$setsolve(m)                    ## send result to cache
  m                                ## output result
}  


## This a function that creates a special matrix and caches it's inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## Setting the value of the matrix
  set <- function(y){
    x <<- y
    i <<-NULL
  }
  ## Getting the value of the matrix
  get <- function() x
  ## Setting the value for the inverse of the matrix
  setsolve <- function(solve) i<<- solve
  ## Getting the value for the inverse of the matrix
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


##  This cache first checks to see if the inverse has been calculated. If so, it gets the inverse from the cache and skips the computation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}

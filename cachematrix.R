## These functions creates a cached value of the inverse of a matrix,
## so instead of having to recalculate the inverse of a matrix over
## and over again, these functions allow the inverse matrix to be
## saved in a new environment so the computer can simply look up 
## the value rather than having to recalculate the inverse of the
## matrix every single time.

## This function constructs a list of functions. Two of the functions
## allow you to access the value of either the orginal matrix, or the
## value of the inverted matrix. The other functions set the value 
## of the matrix and the inverse matrix. 

makeCacheMatrix <- function(x = matrix())
  {
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() 
  {
    x
  }
  setinverse <- function(solve)
  {
    m <<- solve
  }
  getinverse <- function() 
  {
    m
  }
  list(set = set,
       get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}
  

## This function checks weather or not an inverse matrix has already
## been calculated. it then returns the cached value if one exists. 
## if not, the functions calculates the inverse and caches the result
## before returning the final value. So if the function were to be
## called again, the value of the inverse matrix will already be 
## cached.
cacheSolve <- function(x, ...) 
  {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  }

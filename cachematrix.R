## makeCacheMatrix function has four internal functions: 
## set-get to save the values of x and inv parameters in a global enviroment   
## setInverse-getInverse functions save the matrix inverse and save if it does not exist
## and finally create a "matrix" list 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve function asks for the matrix inverse if this exist at the global variable, 
## if yes shows a message info that uses cache and give the value (matrix inverse) 
## if no calculate the matrix inverse with solve function
## and finally shows it

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

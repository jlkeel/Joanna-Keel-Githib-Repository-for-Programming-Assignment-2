##Creates a matrix containing a function that sets the value of the matrix and 
##gets its value, then sets the value of the inverse and gets its value


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m <<- 
    getInverse <- function() m 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


##Computes the inverse of the matrix created above, checking first if the 
##inverse has already been calculated, and if it has it skips the computation
##and retrieves the inverse from the cache. Otherwise, it computes the inverse 
##of the data and sets the value of the inverse in the cache using the 
##setInverse function


cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}


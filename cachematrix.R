## These are functions to create a matrix and to store/calculate
## its inverse (all assumed invertible)
  
## This function stores a matrix and its inverse and
## some functions to deal with it, as a list
makeCacheMatrix <- function(x = matrix()) 
{
  # inv is the inverse of this matrix
  # it is not calculated yet by default
  inv <- NULL            
  
  # set this matrix to matrix y passed in
  # with a NULL inverse
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  # returns this matrix
  get <- function()     #return the matrix
  {
    x
  }
  
  # sets the inverse of this matrix to the one passed in
  setinv <- function(inverse)
  {
    inv <<- inverse
  }
  
  # returns the inverse of this matrix
  getinv <- function()
  {
    inv
  }
  
  # this is where all of the above is stored
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function calculates the inverse of a matrix
## and also will check to see if it is cached before doing so
cacheSolve <- function(x, ...) 
{
  # gets the inverse from the matrix passed in
  inv <- x$getinv()
  
  # checks to see if the inverse is calculated
  if(!is.null(inv))
  {
    # if it's calculated (not null) use the existing value
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculate it
  
  # get the matrix
  data <- x$get()
  
  # get its inverse
  inv <- solve(data)
  
  # set the inverse
  x$setinv(inv)
  
  # return the inverse
  inv
}

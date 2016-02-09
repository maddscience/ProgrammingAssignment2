## Function to Cache the input matrix and provide inverse 
of the matrix using solve function


## First function where an input matrix is received
makeCacheMatrix <- function(ipMatrix = matrix()) 
{
  set <- function(y) ipMatrix <<- y
  get <- function() ipMatrix
  
  invrse <- NULL
  
  getInverse <- function() invrse
  setInverse <- function(inverse) invrse <<- inverse
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}

## Second function where the matrix is inversed using solve function
cacheSolve <- function(ipMatrix, ...) 
{
  
  # Getting the Matrix from Matrix Cache function
  invrse <- ipMatrix$getInverse()
  
  if (!is.null(invrse)) 
  {
    message("Cached Matrix Data")
    return(invrse)
  }
  
  matix <- ipMatrix$get()
  invrse <- solve(matix) # Inverse of the matrix
  ipMatrix$setInverse(invrse)
  invrse
  
}
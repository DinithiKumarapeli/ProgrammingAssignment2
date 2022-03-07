## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL#initializing inverse to NULL
  set <- function(y) { #Set the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x #get the matrix
  setInverse <- function(inverse) inv <<- inverse#set the inverse of the matrix
  getInverse <- function() inv#get the inverse of the matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) { #check if the inverse has been already calculated
    message("getting cached data")
    return(inv)#retrieve the inverse from the cache
  }
  mat <- x$get()#get the matrix
  inv <- solve(mat, ...)#Calculate the inverse of the matrix
  x$setInverse(inv)
  inv#print the inverse matrix
}

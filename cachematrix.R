## The functions below will be used to cache the inverse of a matrix. 

## makeCacheMatrix creates a list of functions that set and get the matrix itself and that
## set and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  set <- function(y) {
    x <<- y
    xInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) xInverse <<- inverse
  getinverse <- function() xInverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function uses the special list created by makeCacheMatrix to either calculate the inverse of the 
## given matrix (if it has not been calculated before) or it fetches the cached value of this inverse.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinverse(inverse)
  inverse
}



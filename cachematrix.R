# A pair of functions that cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

# 1. set the matrix 
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <<- NULL

  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  # return a list 
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  
}


# cacheSolve computes the inverse of the special ``matrix" returned by 
# makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse() 
  
   # check if the inverse has already been calculated, then retrieve the inverse.
   if(!is.null(m)){
    message("getting cached data")
    return(m)
   }
   data <- x$get()
   m <- solve(data)
   x$setinverse(m)
   return(m)
}



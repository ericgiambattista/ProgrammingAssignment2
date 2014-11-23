### These functions (cachematrix.R and cacheSolve.R) cache the value 
### of a matrix inverse, storing the value of the inverse the first 
### time a new matrix inverse is computed so that the inverse does 
### not need to be computed again.

### cachematrix.R creates a special "matrix" object that caches a matrix 
### and it's inverse, i.e. sets and gets the value of a matrix and its inverse
### for speeding up computation of large matrix inverses

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
         ## These assignment operators search through the parent environments
         ## for an existing definition of x and m. If there is an existing 
         ## definition the value is redefined, otherwise assignment takes place
         ## in the global environment.
         x <<- y
         m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get, setinverse = setinverse, getinverse=getinverse)
}

### cacheSolve.R gives the matrix inverse for a matrix - if the matrix
### inverse has already been computed, it gives the inverse from the cache,
### otherwise it computes the inverse (which will then store the inverse in the cache 
### for future computations)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## If the inverse has already been computed, get the cached inverse
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  ## If the inverse has not been computed, use makeCacheMatrix to cache the value
  ## of the matrix inverse to shorten future computation
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

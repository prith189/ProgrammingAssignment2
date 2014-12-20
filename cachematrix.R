## These two functions are used to create a matrix object
## and cache its inverse


## MakeCacheMatrix returns a list of functions to Set a Matrix(set), 
## Get a Vector(set), Set the inverse of a Matrix(setinverse),
## Get the inverse of a Matrix(getinverse)

makeCacheMatrix <- function(x = matrix()) {
  ## Define the functions to set a matrix, get a matrix, 
  ## set the inverse of a matrix, get the inverse of a matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## CacheSolve returns the inverse of a matrix.
## It first checks if the inverse has already been computed. If so, returns the cached matrix
## If not cached already, it computes the inverse and saves it in the Cache

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## If inverse already computed, return the Cached value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Else, compute inverse and save it in cache and return the inverse
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  

}

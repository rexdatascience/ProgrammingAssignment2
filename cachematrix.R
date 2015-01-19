## makeCacheMatrix create a special "matrix" object that can cache its inverse
## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix

## makeCacheMatrix creates a special "matrix" that stores a matrix and cache its inverse.
## It takes a matrix object as parameter and returns a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # m stores the inverse of the matrix
  m <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  
  # get the invere of the matrix
  getinverse <- function() m
  
  # return list object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" created by makeCacheMatrix
## It takes the list object returned by makeCacheMatrix as parameter and returned the inverse of the matrix.
## It checks if the inverse of the matrix is cached and returns the cached value if available,
## otherwise it will compute the inverse and cached the value in the list object.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  # check if cached value exists, and returns cached value if available
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # no cached value, get the matrix and compute inverse
  data <- x$get()
  m <- solve(data)
  
  # cached the inverse and return the inverse 
  x$setinverse(m)
  m
}

## We need two variables 
## 1. x - To hold the matrix 
## 2. inv - To hold it's inverse

## We need four functions 
## 1. set the matrix 
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse

## similar to the example we need a list that we can return from the function 
## The list is basically a list of four functions 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(invInput) inv <<- invInput
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)}


## The function first calls getInverse on the provided input matrix
## If the inv of that inputMatrix is not NULL then it will return that value
## If the inv is NULL then it will compute the inverse by calling solve method
## and then set the inv for the inputMatrix and return the inverse
cacheSolve <- function(x, ...) {
  inv <- x$getInverse() 
  if (!is.null(inv)) {
    message("returning the cached inverse of the matrix")
    return(inv)
  }
  
  message("computing the inverse of the matrix")
  orig_matrix <- x$getMatrix()
  inv <- solve(orig_matrix)
  x$setInverse(inv)
  inv
}

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

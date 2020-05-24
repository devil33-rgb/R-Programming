## At First We will Create A special Matrix
makeCacheMatrix <- function(x = matrix()){
  ##At first inv is set to be NULL 
  inv <- NULL
  set <- function(y){
    ##here value Of x is assigned to y
    x <<- y
    inv <<- NULL
  }
  ## 'get' : is used to retrieve the matrix from set function
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  ## 'getInverse' : is used to retrieve the Inverse matrix from set function
  getInverse <- function() inv
  list(set = set ,
       get = get , 
       setInverse = setInverse ,
       getInverse = getInverse)
  
}
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##mat : is retrieving the data from get function
  mat <- x$get()
  ##here the inverse of matrix by using function 'solve' is assigned to inv
  inv <- solve(mat, ...)
  ## now here 'inv' value is updated in 'setInverse' function , After which the inverse value is storwd in setInverse function which caan be used as cache
  x$setInverse(inv)
  inv
}











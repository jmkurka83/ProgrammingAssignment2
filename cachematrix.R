## ASSIGNMENT - 2 FUNCTIONS TO CACHE THE INVERSE OF A MATRIX
#Creates a special "vector" (a list with a function to:
#  1. set the value of the matrix
#  2. get the value of a matrix
#  3. set the value of the inverse
#  4. set the value of the inverse
makeMatrix <- function(x = matrix()) {
  m < NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(x) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#Calculates the inverse matrix created above
#     First checks to see if inverse has already been calculated
#     If so, it gets inverse from the cache and skips computation
cacheinverse <- function(x,...){
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}




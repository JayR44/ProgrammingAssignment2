makeCacheMatrix <- function(x = matrix()) {
  
  ##Creates a special "matrix", which is really a list containing a function to:
  # 1) set the value of the matrix
  # 2) get the value of the matrix
  # 3) set the value of the inverse
  # 4) get the value of the inverse
  
  m <- NULL
  
  #set the value of the matrix
  set <- function(y){    
    x <<- y
    m <<- NULL
  }
  
  #get the value of the matrix
  get <- function() x
  
  #set the value of the inverse
  setinverse <- function(solve) m <<- solve
  
  #get the value of the inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
  

cacheSolve <- function(x, ...) {

  ##Calculates the inverse of a matrix created with the `makeCacheMatrix` function.
  #However, it first checks to see if the inverse has already been calculated.
  #If so, it gets the inverse from the cache and skips the computation.
  #Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setmean function.
  
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}

## Test

q1 <- matrix(c(1,2,3,4),2,2)
qm1 <- solve(q1)
cm1 <- makeCacheMatrix(q1)
cacheSolve(cm1)

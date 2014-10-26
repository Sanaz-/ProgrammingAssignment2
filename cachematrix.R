## makeCacheMatrix function set the value for matrix, return the matrix, set the value for inverse matrix and 
## return the inverse matrix

makeCacheMatrix <- function(m=matrix()){
  
  inv <- NULL
  set <- function(x){
    m <<- x
    inv <<- NULL
  }
  
  get <- function() m
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}

## cacheSolve checks if the inverse matrix is already calculated, returns the inverse matrix with the message,
## and if there is no calculated inverse matrix, it inverses the matrix and return the inversed matrix.

cacheSolve <- function(m){
  inv <- m$getinverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  
  ma <- m$get()
  inv <- solve(ma)
  m$setinverse(inv)
  inv
}
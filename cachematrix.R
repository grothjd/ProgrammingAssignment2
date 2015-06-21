## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invm<-NULL
  set<- function(y){
    x<<- y
    invm<<- NULL
  }
  get<- function() x
  setinvm<- function(inversematrix) invm<<- inversematrix
  getinvm<- function() invm
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinvm()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinvm(invm)
  invm
}

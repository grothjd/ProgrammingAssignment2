## Creates an object to store a matrix 
## and calculate and stores the inverse of the matrix


## makeCacheMatrix() is a fuction that creates a list of fuctions:
## creates a cache matrix for the inverse of matrix (x)
## invm is the inverse of the matrix (x)
## set is a fuction that sets the matrix x to a given matrix y changing the matrix to inverse
## get retrives the input matrix (x)
## setinvm sets the inverse matrix (invm) to a set value
## getinvm retrieves the inverse matrix (invm)

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


## Determines if the inverse of the matrix has been calculated
## if it has been calculated it returns the inverse of the matrix (invm)
## if it has not been calculated it calculates the inverse of the matrix using solve()
## and returns the inverse of the matrix
## input is the list of fuctions created in the output of the makeCacheMatrix() fuction

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

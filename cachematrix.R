## The first function makeCacheMatrix creates a list of functions to 
## 1. set a matrix from the passed matrix and reset a matrix to NULL
## 2. get the given matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(solve) m<<- solve
  getinv<-function() m
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
  
}

## The second function cacheSolve returns the inverse of a matrix 
## if it runs for the first time and saves the inverse in the cache
## returns the inverse with a message if there is already a saved one in cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinv(m)
  m
}

## Trying the above functions execution.
sample <- matrix(1:4,2,2)
inv <- makeCacheMatrix(sample)
cacheSolve(inv)
inv$getinv()
cacheSolve(inv)
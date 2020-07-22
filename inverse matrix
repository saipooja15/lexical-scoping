makecacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<- nNULL
  }
  get <- function() {x}
  setInverse <- function(invers) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}
cachesolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

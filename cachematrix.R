
# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y){
    x <<- y
    inversa <<- NULL
  }
  get <- function(){
    return (x)
  }
  
  setInverse <- function(inverse){
    inversa <<- inverse  
  } 
  getInverse <- function() {
    return (inversa)
  }
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#The function below calculates the inverse matrix of x if it has not been calculated, 
# otherwise it returns the inverse of the stored result.
cacheSolve <- function(x, ...) {
  
  inversa <- x$getInverse()
  
  if (!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  
  matriz <- x$get()
  inversa <- solve(matriz, ...)
  x$setInverse(inversa)
  inversa
}

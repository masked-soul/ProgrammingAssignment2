## Function to cache the inverse using a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  ##Creating a blank matrix
  a <- NULL
  ##Setting the value of the matrix
  set<-function(y){
   ## Using double arrow assignment operator to make changes in the parent function 
    x <<- y
    a <<- NULL
  }
  get<- function(){x}
  set_inverse <- function(inverse){a <<- inverse}
  get_inverse <- function(){a}
  list(set = set,get = get,set_inverse = set_inverse,get_inverse = get_inverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed)
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ##Getting inverse value form above function 
  a <- x$get_inverse()
  ##Condition to check if inverse has already been calculated
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  matx <- x$get()
  a <- solve(matx,...)
  x$set_inverse(a)
  a
}

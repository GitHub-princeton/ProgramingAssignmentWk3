##creates a special “matrix”, which is really a list containing a function to:
##set   set matrix
##get   set matrix
##setInv  set inverse
##getInv  set inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() i
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)}

##This function computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse  already been calculated, then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(inv)
}
  

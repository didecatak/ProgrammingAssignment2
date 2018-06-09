## This is the assigment that I tried to practise using cache in terms of the lexical scoping rules. 
## makeCachematrix is where I define the getters and setters. 

makeCacheMatrix<- function(x = matrix()) {
  t <- NULL
  set <- function(y) {
    x <<- y
    t <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) t <<- inverse
  getinverse <- function() t
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#this is the cache function which uses the output of the makeCachematrix as an argument#
cacheSolve<- function(x, ...) {
  t <- x$getinverse()
  if(!is.null(t)) {
    message("getting cached data")
    return(t)
  }
  data <- x$get()
  t <- solve(data, ...)
  x$setinverse(t)
  t
}

#this the part I make an example. I create 2 by 2 matrix. and printed out. then I used the argument in makeCachematrix to inverse it.

D <- matrix(c(2,0,1,3),2,2)
D
I1 <- makeCacheMatrix(D)

#in this part I used cashsolve to get the inverse matrix. At first run, it is not from cache. but after the first one all of the results came from cache.
cacheSolve(I1)
#second run
cacheSolve(I1)
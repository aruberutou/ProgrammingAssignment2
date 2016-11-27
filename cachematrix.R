## Put comments here that give an overall description of what your
## functions do

#' This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {m <<- inverse}
  getinverse <- function() {m}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#' This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#' If the inverse has already been calculated (and the matrix has not changed), 
#' then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m    
}

# ## Tests
# 
# madeCacheMatrix <- makeCacheMatrix() #ok; returns value object
# 
# matr <- rbind(c(1, -1/4), c(-1/4, 1))  # create invertible matrix
# det(matr) # non-zero determinant == invertible
# solve(matr) # confirmed to be invertible
# 
# 
# madeCacheMatrix$set(matr) #ok; no error
# madeCacheMatrix$get() #ok; returns matrx
# 
# cacheSolve(madeCacheMatrix) #ok; no error
# 
# identical(cacheSolve(madeCacheMatrix), solve(matr)) #ok; returns true

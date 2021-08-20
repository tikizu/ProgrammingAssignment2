## Coursera course R programming week 3 peer graded assignment
## Developer tikizu
## Github: https://github.com/tikizu/ProgrammingAssignment2

##Task: Caching the Inverse of a Matrix


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.makeCacheMatrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setsolve(m)
  m
}


## Testing the functions with a simple 2x2 matrix
test010 <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(test010)
cacheSolve(test010)
## Testing the functions with a big 100x100 matrix
random <-runif(n=100000, 0, 100)
test020 <- makeCacheMatrix(matrix(random,100,100))
cacheSolve(test020)

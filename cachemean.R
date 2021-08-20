#################################################################
## Coursera course R programming week 3 peer graded assignment
## Developer tikizu
## Github: https://github.com/tikizu/ProgrammingAssignment2
#################################################################


##These functions are privided in the Assignment as an examples

##Creates a vector which contains a function

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## Calculates the mean of the special vector created with makeVector
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Testing the two given functions

test010 <- makeVector(1:10)
test020 <- makeVector(1:10)
test030 <- makeVector(2)

cachemean(test010)
cachemean(test010)

cachemean(test020)

cachemean(test030)

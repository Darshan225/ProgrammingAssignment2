## makeCacheMatrix Function generate a special type of matrix which can store its inverse where as
## fcacheSolve function compute the inverse if the inverse is never calculated before or else it fetches the calculated inverse 

## Function generate a matrix able to store its inverse

makeCacheMatrix <- function(x = matrix()) {
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


## Calculates/Fetches the inverse 

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

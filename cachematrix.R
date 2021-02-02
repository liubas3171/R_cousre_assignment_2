
makeCacheMatrix <- function(x = matrix()) {
  # Returns a list of functions. With it's help one can find info about matrix
  res <- NULL
  set <- function(y) {
    x <<- y
    res <<- NULL
  }
  get <- function()
    x
  set.inverse <- function(solve)
    res <<- solve
  get.inverse <- function()
    res
  list(
    set = set,
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse
  )
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  res <- x$get.inverse()
  # check if inverse is already calculated
  if (!is.null(res)) {
    message("getting cached data")
    return(res)
  }
  data <- x$get()
  # Calculate inverse
  res <- solve(data, ...)
  x$set.inverse(res)
  res
}

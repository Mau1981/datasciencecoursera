## These functions calculate the inverse of a matrix and returned it from cache we it is retrieved without caclulating it again

## Store the inverse of a Matrix (es matrix(1:2,2,2))

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setinv <- function(invert) m_inv <<- invert
  getinv <- function() m_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Calculate the inverse of matrix if it hhasn't been yet calculated. otherwise it get the data from cache by using the above function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m_inv <- x$getinv()
  if(!is.null(m_inv)) {
    message("getting cached inverted Matrix")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)
  x$setinv(m_inv)
  m_inv
}

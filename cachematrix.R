# Create cached matrix
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      # Function for setting the matrix
      set <- function(y) {
            x <<- y
      # Since we change the matrix we need to set the inverse to NULL
      # Because we haven't computed it for new matrix
            m <<- NULL
      }
      # Function to return the matrix we have stored
      get <- function(){x}
      # Sets m to the inverse of our matrix
      setinverse <- function(inverse){m <<- inverse}
      # Returns m (Null when we didn't compute yet and inverted m after
      # computation)
      getinverse <- function(){m}
      # Return list of functions
      return(list(set = set, get = get, setinverse = setinverse,
           getinverse = getinverse))
}
cacheSolve <- function(x, ...) {
      # Get matrix from our caching function
      m <- x$getinverse()
      # If m is not empty we just get already computed value
      # Print message so we know we are using cached data
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      # Get the matrix form our cachign function
      data <- x$get()
      # Solve the matrix for inverse
      m <- solve(data, ...)
      # Cache the result
      x$setinverse(m)
      # And return it
      return(m)
}

#
# Assignment: Write the following functions:
#  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.



# #establish list of environments to create and reference the matrix inverse. 
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL  # create m object
  set <- function(y) {               #set function: bring matrix to global environment
    x <<- as.matrix(y)
    m <<- NULL
  }
  get <- function() x                #get function: retrieve matrix 
  setinv <- function(inverse) m <<- inverse      #setinv to write the inverse matrix
  getinv <- function() m             #getinv to retrieve the inverse matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##actually calculate the inverse (or return a cached version)


cacheSolve <- function(x, ...) {
  m <- x$getinv()              #check if inverse already defined, then calc if needbe
  if(!is.null(m)) {
    message("getting cached data")
    return(m)             #return matrix inverse and exit function
  }                      #otherwise....
  data <- x$get()        #retrieve input matrix
  m <- solve(data, ...)  #calculate the inverse
  x$setinv(m)            #set the inverse using the setinv function
  m                      #output the newly calculated inverse
  
}

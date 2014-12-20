## This function sets the matrix, then gets the matrix, 
##then solves for the inverse and then gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
             set <- function(y) {
                     x <<- y
                     m <<- NULL
             }
             get <- function() x
             setinv <- function(solve) m <<- solve
             getinv <- function() m
             list(set = set, get = get,
                  setinv = setinv,
                  getinv = getinv)
     
 }
##This function looks to see if an inverse has already been calculated for a matrix.
##If it has, the cached value is returned. If not it calculates the inverse 
##and stores the value. 

cacheSolve <- function(x, ...) {
 m <- x$getinv()
             if(!is.null(m)) {
                     message("getting cached data")
                     return(m)
             }
             data <- x$get()
             m <- solve(data, ...)
             x$setinv(m)
             m
     }

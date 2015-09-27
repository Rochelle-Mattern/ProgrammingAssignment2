## This file contains two main functions, makeCacheMatrix and cacheSolve. 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## The makeCacheMatrix has four sub-functions: set, get, setinv, and getinv. ## The set function sets the inv variable to NULL and sets it as an object and sets x to an object and to the input y
## The get function simply returns x
## The setinv function takes the inverse as an arguement and sets it as an object inv
## The getinv returns inv
## The makeCacheMatrix ends by listing each function in the makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse){
		inv <<- inverse
	}
      getinv <- function() inv
      
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve takes in a matrix x and checks if it's inverse is cached, if it is then it tells you its retreived the cached data and returns it. If not, it calculates the inverse, caches it, and returns the inverse. 

cacheSolve <- function(x, ...) {
       inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix_x <- x$get()
        inv <- solve(matrix_x, ...)
        x$setinv(inv)
        inv
}

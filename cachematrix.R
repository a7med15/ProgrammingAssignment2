## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix rather
## than compute it repeatedly.
## The following two functions are used to cache the inverse of a matrix:
## 1. The function 'makeCacheMatrix' creats a special "matrix" object
##    to catch its invers.
## 2. The function 'cacheSolve' computes the inverse of the special "matrix" returned
##    by makeCacheMatrix function.
##    If the inverse has already been calculated and the matrix has not changed, 
##    then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix, set()
## 2. get the value of the matrix, get()
## 3. set the value of inverse of the matrix, setinverse()
## 4. get the value of inverse of the matrix, getinverse()

makeCacheMatrix <- function(x = matrix()) {
                  nv <- NULL
                  set <- function(y) {
                  x <<- y               # superassignment operator: <<-use `<<-`  is
                  nv <<- NULL           # to assign a value to an object in an environment 
                                        # different from the current environment
                  }
                get <- function() x
                setinverse <- function(inverse) nv <<- inverse
                getinverse <- function() nv
                list (set = set, get = get, setinverse = setinverse,
                      getinverse = getinverse)
} 

## Write a short comment describing this function
## 'cacheSolve' returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache through
## setInverse function.

cacheSolve <- function(x, ...) {  
    nv <- x$getinverse()          
  if (!is.null(nv)) {             ## if the inverse has already been calculated
    message("getting cached data") ## get it from the cache and skips the computation.
    return(nv)                    ## Return the inverse of a matrix 'x'
  }
  m.data <- x$get()               ## otherwise, it calculates the inverse
  nv <- solve(m.data, ...)        ## solve matrix and stor it in 'nv'
  x$setinverse(nv)                ## sets the value of the inverse in the cache
                                  ## through the setInverse function.
  nv                              
}

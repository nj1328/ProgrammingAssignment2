## Programming Assignment 2: Lexical Scoping

## This pair of functions caches the inverse of a matrix.

## The first function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {

    ## initialise minv as null - this will be the inverse of matrix x
    minv <- NULL

    ## Assign input argument (y) to x in the parent environment.
    ## Assign NULL to minv in the parent environment 
    ## to clear any previously cached value.
    
    set <- function(y) {
      x <<- y
      minv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inv) minv <<- inv
    getinverse <- function() minv

    ## create an object of type makeCacheMatrix()
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    ## if the cached matrix inverse is not NULL, return cached value in minv
  
    minv <- x$getinverse()
    if(!is.null(minv)) {
      message("getting cached data")
      return(minv)
    }
    
    ## if the cached matrix inverse is NULL, get the matrix from the
    ## input object, calculate the inverse using solve() function
    ## and set the value in the cache
    
    data <- x$get()
    minv <- solve(data, ...)
    x$setinverse(minv)
    minv
}

## function makeCacheMatrix: 
##     save and retrieve matrix and its inverse in cache
## 
## function cacheSolve:
##     retrieve the inverse of a matrix from cache if it's available
##     otherwise, compute the inverse, save in cache, and returns 
##     the inverse


## This function initializes the inverse of a matrix to NULL in cache
## and provide the following four functions
## get - get the matrix
## set - set the matrix in cache, and initialize the inverse in cache
## getmatrix - get the inverse from cache
## setmatrix - save the inverse into cache

makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverse matrix to NULL
    m <- NULL
    
    # set the matrix, and initialize the inverse to NULL
    # both the matrix and the inverse matrix refer to the
    # cache memory
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    # get the matrix
    get <- function() x
    
    # set the inverse matrix to the cache
    setmatrix <- function(solve) m <<- solve
    
    # get the inverse matrix from the cache
    getmatrix <- function() m
    
    # list the available functions
    list( set = set, 
          get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix )
}


## This function first tries to get the inverse of a matrix x
## from cache memory. It returns the inverse if the inverse
## is in the cache, otherwise, it gets the matrix x from the 
## cache, computes the inverse, saves the inverse in cache,
## and returns the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    
    # if the inverse is in the cache, return it
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    # if the inverse is not in cache, get the matrix 
    # from the cache, compute the inverse, put it in 
    # cache, and return the inverse
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m    
}

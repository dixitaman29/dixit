## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss 
## here). Your assignment is to write a pair of functions that cache the inverse
## of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(cached_mat = matrix()) {
    cached_inv <- NULL
    
    setmat <- function(mat) {
        cached_mat <<- mat
        inv <<- NULL
    }
    
    getmat <- function() cached_mat
    
    setinv <- function(inv) cached_inv <<-inv 
    
    getinv <- function() cached_inv
    
    list(setmat = setmat,
         getmat = getmat,
         setinv = setinv,
         getinv = getinv)

}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above

cacheSolve <- function(cache, ...) {
    inv <- cache$getinv()
    
    if(!is.null(inv)) {
        message("using cache")
        return(inv)            
    }

    mat <- cache$getmat()
    inv <- solve(mat, ...)
    cache$setinv(inv)
    
    inv
}

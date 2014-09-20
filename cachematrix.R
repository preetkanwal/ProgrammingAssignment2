## function makeCacheMatrix takes a new square matrix whose inverse needs to be calculated
## and returns a list of functions setmat, getmat, setinv, getinv
## setinv - to set the value of inverse matrix to the cache
## getinv - to fetch the value of inverse matrix to the cache
## setmat - to set the matrix whose inverse needs to be calculated
## getmat - to fetch the matrix whose inverse needs to be calculated

makeCacheMatrix <- function(mat = matrix()) { # 1x1 NA Matrix is provided as default argument
    invmat <- matrix()
    
    setmat <- function(y) {
        mat <<- y
        invmat <<- matrix()
        }
    
    getmat <- function()    return(mat)
    
    setinv <- function(k)   invmat <<- k
    
    getinv <- function()    return(invmat)
    
    list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}


## cacheSolve takes a square matrix and checks whether inverse already exists in the cache
## In case inverse doesnot exists in the cache, it computes inverse and stores the value
## in the cache for future reference using x$setinv function
## x$getinv() - checks previously stored value of inverse matrix
## x$setinv() - stores the value of inv matrix to the cache for future reference

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmat <- x$getinv()
    
    if(!is.na(invmat[1,1]))  {
        print("get cached value of inverse matrix")
        return(invmat)
    }
    mat <- x$getmat()
    invmat <- solve(mat, ...)
    x$setinv(invmat)
    return(invmat)
}

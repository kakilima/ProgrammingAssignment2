## Implementation of Matrix Inverse Calculation with caching

## Function makeCacheMatrix - Store matrix object

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## set/get function - store or retrive matrix
    set <- function(y) {
        x <- y
        inv <- NULL
    }
    get <- function() x
    
    ## getinv/setinv function - store or retrive matrix inverse
    setinv <- function(matrixinverse) inv <<- matrixinverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function cacheSolve - calculate or retrive cached matrix inverse

cacheSolve <- function(x, ...) {
    ## Check for cached matrix inverse
    
    ## If exist, return cached version
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Retriving cached matrix inverse")
        return(inv)
    }
    
    ## Else, calculate inverse, store and return
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

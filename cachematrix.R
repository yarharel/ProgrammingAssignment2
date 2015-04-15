
## creates a list of functions of getters and setters to the matrix value (-1) value
makeCacheMatrix <- function(x = matrix()) {
    cS <- NULL 
    set <- function(y) { 
        x <<- y
        cS <<- NULL
    }
    get <- function() x 
    setS <- function(cacheS) cS <<- cacheS  
    getS <- function() cS 
    list(set = set, get = get,
         setS = setS,
         getS = getS)
}


## function returning the x(-1) from cache if it exists, 
##  otherwise calculating it and storing it in cache
cacheSolve <- function(x , ...) {
    cS <- x$getS() 
    if(!is.null(cS)) { 
        message("getting cached data")
        return(cS)
    }
    data <- x$get() 
    cS <- solve(data, ...) 
    x$setS(cS)
    cS
}
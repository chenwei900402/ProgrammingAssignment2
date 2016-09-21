## Chen Wei
## Cache the Matrix for Inversion

## Create Object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
       if(as.integer(sqrt(length(x))) != sqrt(length(x))){
               message('This is not a square Matrix')
       } else {
               dim(x) <- c(sqrt(length(x)),sqrt(length(x)))
       }
        w <- NULL
        set <- function(y){
                x <<- y
                w <<- NULL
        }
        get <- function()x
        setsolve <- function(solve){ w <<- solve}
        getsolve <- function() w
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)    
}


## Do the inversion

cacheSolve <- function(x, ...) {
        w <- x$getsolve()
        if(!is.null(w)){
                message('getting cached data')
                return(w)
        }
        data <- x$get()
        w <- solve(data)
        x$setsolve(w)
        w        
}

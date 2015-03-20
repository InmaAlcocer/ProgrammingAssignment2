##set the value of the matrix
##get the value of the matrix
## set the value of the inverse
##get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {  
                x <<- y
                invs <<- NULL
        } 
        get <- function() x  
        setinvs <- function(invs) invs <<- invs 
        getinvs <- function() invs 
        list(set=set, get=get, 
             setinvs=setinvs, 
             getinvs=getinvs)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) { 
        invs <- x$getinvs()
        if(!is.null(invs)) {
                message("getting cached data.")
                return(invs)
        }
        data <- x$get()
        invs <- solve(data)
        x$setinvs(invs)
        invs
}


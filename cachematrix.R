## The two functions below allow to set the value of a matrix, calculate and cache its inverse.
## Once the value of the matrix is set, the functions either compute the matrix inverse
## or get its value from the cache (if already calculated) and skip the inverse computation.

## The function below allows for a matrix to: 1)set its value 2) get its value
## 3)set the inverse 4) get the inverse  
makecachematrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() {
            x
        }
        setinverse <- function(inverse) mi <<- inverse
        getinverse <- function() mi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function below calculates the inverse of a matrix but it first checks if the inverse 
## was already calculated, if so, gets the inverse from the cache and skip the computation.
cachesolve <- function(x, ...) {
        mi <- x$getinverse()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setinverse(mi)
        mi
}
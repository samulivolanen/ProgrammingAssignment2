## R Programming Assignment 2: Lexical Scoping. The functions created in this
## assignment will cache the inverse of a matrix. 

## The function below creates a matrix that inverses its cache

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function below gets the inverse of the matrix created above.
## If the inverse has been calculated the function gets the inverse from the
## cache: "getting cached data". 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver
}

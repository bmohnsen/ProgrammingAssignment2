## MakeCacheMatrix - Function creates a special"matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {

	#set the value of the matrix
	inv <- NULL
    	 set <- function(y) {
                x <<- y
                inv <<- NULL
        }

	#get value of the matrix
     	   get <- function() x

	#set value
       	 setinverse <- function(mat) inv <<- inverse

	#get value
     	   getinverse <- function() inv

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve - computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated, then cachesolve retrieves the inverse


cacheSolve <- function(x, ...) {
        ## Get inverse if available
        m <- x$getinverse()
        

        if(!is.null(m)){
                message("getting data")
                return(m)
        }
        ##else
        data <- x$get()
        m <- inverse(data, ...)
        x$setinverse(m)
        m
}

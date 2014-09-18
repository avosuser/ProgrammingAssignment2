## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # set i "inverse" to NULL
    i <- NULL
  
    # the y argument in the set function initializes 
    # x from the parent env.
    set <- function(y = matrix()) {
    x <<- y
    i <<- NULL
    }
  
    # the get function just returns x
    get <- function() x
  
    # the setinv function just sets the value of i either from the parent environment or global environment
   setinv <- function(inverse) i <<- inverse
   
   getinv <- function() i
  
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		if(!is.null(inv)) {
			message("Getting cached data")
			return(inv)
		}
  
		data <- x$get()
  
		if(length(data[,1]) != length(data[1,])) {
			stop("Inverse of a matrix can be done only on a square matrix")
		}
   
		inv <- solve(data)
		x$setinv(inv)
		inv		
}

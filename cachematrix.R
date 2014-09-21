## The makeCachematrix functions sets up all the variables to cache the inverse of a matrix.
## The function returns a list of functions to get and set (the initial) matrix and to set and get the inverse of the matrix.
## How to run this program initialize a sqaure matrix say m <- matrix(c(1,4,5,10,12,16,3,6,9), nrow=3, ncol=3)
## pass m to the makeCacheMatrix as an argument tt <- makeCacheMatrix(m)
## Then run cacheSolve by passing tt i.e. cacheSolve(tt), as the inverse of m has not been calculated as yet you will see the inverse being output
## If you run it oncemore i.e. cacheSolve(tt) you will see the output with the message "Getting cached data"

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


## The cacheSolve function calculates the inverse of a square matrix it only runs the solve() function
## if the inverse is not already caclulated i.e. 'i' in the makeCacheMatrix is 'NULL'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		if(!is.null(inv)) {
			message("Getting cached data")
			return(inv)
		}
  
		data <- x$get()
		# Check if the matrix is a square matrix, if it is not abort the program (I just put this in as extra)
		if(length(data[,1]) != length(data[1,])) {
			stop("Inverse of a matrix can be done only on a square matrix")
		}
   
		inv <- solve(data)
		x$setinv(inv)
		inv		
}

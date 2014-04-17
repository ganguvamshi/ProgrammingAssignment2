## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#  This function creates a special list containing functions to
#  1) set a new matrix
#  2) get the matix values
#  3) set the inverse of matrix
#  4) get the inverse of matrix (if calculated already, else return NULL)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y = matrix()){
                inv <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse = function() inv
        list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}



## Write a short comment describing this function
#  This function returns the inverse of the matrix created by using makeCacheMatrix function
#  Checks for whether the inverse of the matrix is already calculated, if so just returns it from cache 
#  else it calculates the inverse and store it in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("Retreiving Cached Data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

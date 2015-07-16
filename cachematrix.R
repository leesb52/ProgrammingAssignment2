## Put comments here that give an overall description of what your
## functions do
## I am Writing a pair of functions that cache the inverse of a matrix

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
         set <- function(y) {
                x <<- y
                m <<- NULL
         }
         get <- function() x
         setsolve <- function(solve) m <<- solve
         getsolve <- function() m
         list(set = set, get = get,
              setsolve = setsolve,
              getsolve = getsolve)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated, then 
## the cacheSolve will retrive the inverse from the cache rather than 
## calculate again, by which we can save time.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

#Example 1
#y<-matrix(1:4, ncol=2)
#listmat<-makeCacheMatrix(y)
#cacheSolve(listmat)
#cacheSolve(listmat)

#Example 2
#yy<-matrix(c(1,0,3,2,2,4,3,2,1),ncol=3)
#listmat1<-makeCacheMatrix(yy)
#cacheSolve(listmat1)
#cacheSolve(listmat1)


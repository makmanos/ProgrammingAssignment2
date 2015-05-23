## These two functions are supposed to cache the value of the inverse of a square matrix
## The way to use them is :
## 1. First call makeCacheMatrix passing it the square matrix you'd like to cache.
## example: if Z is a square matrix create its cached version like this: ZCached<-makeCacheMatrix(Z)
## 2. The call the cacheSolve() function passing it the cached version of the matrix you 
## previously created. 
## example: cacheSolve(ZCached)
## The 1st time you call cacheSolve, the inverse of the matrix will be null and calculated
## every subsequent call to the cacheSolve function with the same matrix will return the 
## cached inverse value

## makeCacheMatrix accepts a square matrix as an argument and stores the inverse of it in the
## inv variable
## it also provides a list with 4 functions to manipulate the contents of the matrix.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set a standard matrix with n rows and n cols
  set <- function (y){
    x <<- y        
    inv <<- NULL
  }
  get <- function () x
  setinv <- function (mInv) inv <<- mInv
  getinv <- function () inv
  list (set=set, get=get, 
        setinv = setinv,
        getinv = getinv)
}


## cacheSolve accepts a square matrix that has been created with the makeCacheMatrix function above
##for an argument and returns a matrix that is the inverse of it.
## If the inverse has been previously calculated and the matrix hasn't changed, it will
## not calculate it again but rather return a cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)){
        message ("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}

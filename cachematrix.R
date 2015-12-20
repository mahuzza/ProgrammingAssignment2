## makeCacheMatrix creates a list which stores values relating to the inverse of a matrix input. cacheSolve searches the list created by 
##makeCacheMatrix for relevant values - if these are NULL then the inverse is calculated, otherwise the inverse matrix i of x 
##(the input matrix) in the list is returned


## Create a list object which holds the matrix object x, the inverse matrix of x if it has been previously calculated. 
##i is assigned as the object which contains the inverse of the input matrix x. The list contains the following entries:
##get  - holds the matrix x. 
##setinv  - sets the inverse matrix matrix to i. 
##getinv  - gets the inverse matrix i
##returns a list containing these values

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
    set <- function(y) { 
    x <<- y
    i <<- NULL
  }
  get <- function() x 
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve returns the inverse of a square invertible matrix - taking the list object from makeCacheMatrix as an input. If this input has 
##a null listing for the value "i", then the solve function is called and the output is assigned to "i", otherwise the value is returned from 
##the list.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinv() 
  if(!is.null(i)) { #if i value is not null then return list value
    message("getting cached data")
    return(i)
  }
  data <- x$get() #if i value in list == NULL, call solve function to generate inverse and set value in list as this value
  i <- solve(data, ...)
  x$setinv(i)
  i
}

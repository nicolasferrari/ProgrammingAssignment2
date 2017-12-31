## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# In the makeCacheMatrix, first I create a vector, initializing it with the NULL object. Secondly I define
# the set function with the y object as the parameter. Within the scope of the set function I define
# another time the z object

# In the function get, I retrieve the matrix x, and in the function set_inverse
# I assign the object inverse that is the result of the matrix inverse calculation
# correspondingly to the cacheSolve function. Afterwards, the function get_inverse
# take the output of the object z that is defined in the set_inverse function.
# Lastly the function makeCacheMatrix return a list that have four objects. 

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x 
  set_inverse <- function(inverse)  z <<- inverse
  get_inverse <- function() z 
  list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)

}


## Write a short comment describing this function

# The cacheSolve function, take the x parameter, and define the z object as the result of getinverse() function
# in the makeCacheMatrix function. If the result is not null, the cacheSolve function
# return this result that is the inverse of the matrix. This matrix inversion
# was calculated previosuly by the cacheSolve function, and by the line x$set_invserse(z)
# the result of the inverse was set on the setinverse function thas is inside the makeCacheMatrix
# function.

# The second part of the function cacheSolve, get the output of the get() function in the 
# makeCacheMatrix and assign this matrix to the mat object. Then, with the solve built in function
# calculate the inverse of the mat matrix, and input this result in the set_inverse function
# of the makeCacheMatrix. Finally, if the z inverse matrix was not yet calculated, the cacheSolve
# function return the z inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  z <- x$get_inverse()
  if(!is.null(z)){
    print("getting cache data")
    return(z)
  }
  
  inverse <- x$get() 
  z <- solve(inverse,...)
  x$set_inverse(z)
  z
}

u <- matrix(rnorm(1000),nrow=10,ncol=10)
u
m <- makeCacheMatrix(u)

cacheSolve(m)

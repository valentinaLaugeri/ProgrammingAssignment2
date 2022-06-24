## Put comments here that give an overall description of what your
## functions do

## I start by constructing my makeCacheMatrix function
library(MASS) #choice to load library MASS because it allows to use (non)square matrices
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #I make sure this way to set the inverse as NULL to begin with
    set <- function(y){ #setting the value of the matrix
      x <<- y 
      inv <- NULL
    }
    get <- function() x  #function created to get matrix called x 
    setinv <- function(inverse) #setting the value of the inverse
      inv <<- inverse
    getinv <- function(){  #function to get the inverse of previously defined matrix x
              inver <- ginv(x)
              inver%%x        #operator that retrieves the inverse
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## second function called cacheSolve which I will use now to get cache data 

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){ 
      message("getting cached data")
      return(inv) #returns the inverse value if the inverse is not null 
    }
    data <- x$get()
    inv <- solve(data, ....) #this is the crucial step which finds the inverse
    x$setinv(inv)
    inv #final step which is to return "INV", the inverse of the matrix
}


## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## It will create a structure of makeCacheMatrix thats have 4 function, set, get, setInverse and get inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # reset m
  set <- function(y) {   #reset the Martrix with the assigned value
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve #it provide a way to assign value to setInverse in the parent.env
  getInverse <- function() m
  	
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  #returns 4 list item (function)/client function which could be later use

}


## Write a short comment describing this function
## It will return the inverse of the cached matrix x, it make use of the above makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()        #query the x matrix's cache, must use alongs with makeCacheMatrix otherwise return object not found, also it will use cache image of x (function makeCacheMatrix)          
 if(!is.null(m)) {           #if there is a cache
    message("getting cached data") 
    return(m)                #just return the cache, no computation needed
  }
  data <- x$get()            #if there's no cache
  m <- solve(data)           #Get the inverse here
  x$setInverse(m)            #save the result back to x's cache
  m                          #return the result

}

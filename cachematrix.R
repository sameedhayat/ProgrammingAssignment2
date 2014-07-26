##Two functions makeCacheMatrix,cacheSolve that would take care of computing and caching the inverse

##Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x=matrix()){
  
    ##initializing the inverse property
    inverse <- NULL
  
    ##Method to set the value of matrix
    set <- function(y){
      x<<-y
      inverse <<- NULL
    }
  
    ##Method to set the value of matrix
    get<-function() x
  
    ##Method to set the value of inverse matrix
    setinverse <-function(inv){
      inverse <<-inv
    }
  
    ##Method to set the value of inverse matrix
    getinverse <-function() inverse
  
    #Returning the list of methods
    list(set=set,get=get,setinverse = setinverse,getinverse=getinverse)

}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated and the matrix has not changed
## Then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  
    ##getting the value of inverse matrix
    i <- x$getinverse()
  
    ##checking if the inverse is already calculated
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    
    ##getting the matrix
    data <- x$get()
   
    ##calculating the inverse
    i <- solve(data, ...)
  
    #Setting the inverse
    x$setinverse(i)
 
    #Returning the matrix
    i
    
}
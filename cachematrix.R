## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The idea behind this function goes in the same direction as the one used as an example (makeVector) 

makeCacheMatrix <- function(x = matrix()) {
  ## Here we define the set's and get's to the matrix and to the inverse
  ## in the same way as with the mean vector example
  
  ##################################################
  ## First: set the value of the matrix object
    inv<-NULL
    set<-function (y){
    x <<- y
    inv <<- NULL
    }
  ## Second: get the value of the matrix object
    get<-function() x
  ## Third: set the value of the matrix inverse
    setInv<-function(chol2inv) inv<<-chol2inv
  ## Fourth: get  the value of the matrix inverse
    getInv<-function() inv
  ##
  ##################################################
  
  list(set=set,get=get,setInv=setInv,getInv=getInv)

}



## Write a short comment describing this function
## In order to use a more efficient matrix inversion method (instead of SOLVE) here we use Choleski decomposition thorughout 
## chol2inv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## First we check if the inverse has already been computed
  inv<-x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get() ## data is obtained (in this case the matrix)
  inv<-chol2inv(data, ...) ## compute the inverse
  x$setInv(inv) ## cache the inverse
  inv
}  
  
  
  
  


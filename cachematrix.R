## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##generates setters and getters for a matrix and the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinverse <- function(inverse) inv<<-inverse
    getinverse <- function() inv
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)

}


## Write a short comment describing this function

##calculates the inverse of the matrix generated above and sets the value
##of the inverse in the cache via the setinverse() function generated above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<- makeCacheMatrix(x)$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-makeCacheMatrix(x)$get()
    inv<-solve(data,...)
    makeCacheMatrix(x)$setinverse(inv)
    inv
}

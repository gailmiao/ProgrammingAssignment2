##Peer Assessments /Programming Assignment 2: Lexical Scoping **Please Note: No Grace Period**
##Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

##This function creates a special "matrix" object that can cache its inverse called m.
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function()m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## The following function returns to the inverse of the x. 
## First, x$getinverse is to check if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value via the setinverse function.

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}

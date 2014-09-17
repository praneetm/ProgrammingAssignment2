## cachematrix.R
## Programming Assignment 2

## The makeCacheMatrix() function creates a matrix whose inverse can be cached
## Returns a list containing 4 functions:
## set() <- sets the matrix
## get() <- returns the matrix
## setinv() <- sets the inverse for the matrix
## getinv() <- returns the inverse for the matrix
makeCacheMatrix<-function(x=matrix()) {
        inv<-NULL
        
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        
        get<-function(){
                x
        }
        
        setinv<-function(inverse){
                inv<<-inverse
        }
        
        getinv<-function(){
                inv
        }
        
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## The cacheSolve() function calculates,sets and returns the inverse if its 
## not already cached. Otherwise returns the cached value.
cacheSolve<-function(x, ...){
        
        inv<-x$getinv()
        if(!is.null(inv)){
                message("Getting Cached Data")
                return(inv)
        }
        
        inv<-solve(x$get(), ...)
        x$setinv(inv)
        inv
}
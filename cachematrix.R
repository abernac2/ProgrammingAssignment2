##R Programming Assignment 2

##This function creates a special "matrix object that can cache its inverse."
##makeCacheMatrix stores the inverse of of a matrix so it does not have to be recalculated.
##set sets x as a matrix
##get returns the original matrix
##setinverse takes a matrix and sets the inverse
##getmatrix returns the inverted matrix
makeCacheMatrix <- function(mat = matrix()){
        mat <- NULL
        set <- function(m){
                mat <<- m
                invmat <<- NULL
        }
        get <- function(){
                return(mat)
        }
        setinverse <- function(inverse){
                invmat <<- inverse 
        } 
        getinverse <- function(){
                return(invmat)
                }
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache. 
##first you initialize the inverse matrix as null
##then you check whether the object is a matrix or a list
##then you check to see if the inverse of the matrix has already been calculated
##if the inverse has not already been stored, create it
##then store the new inverse
##if there are any issues, return null 
cacheSolve <- function(x, ...) {
        invmat=NULL
        if(class(x)=="matrix"){
                makeCacheObject <- makeCacheMatrix()
                makeCacheObject$set(x)
                invmat <- makeCacheObject$getinverse()
        }
        if(class(x) == "list"){
                makeCacheObject <- x
                invmat <- x$getinverse()
        }
        if(! is.null(invmat)){
                message("obtaining cached inverse vector")
                return(invmat)
        }
        else {
                invmat <- solve(makeCacheObject$get(), ...)
                makeCacheObject$setinverse(invmat)
                return(invmat)
        }
        return(NULL)
}
        
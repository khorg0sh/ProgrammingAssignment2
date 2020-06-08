## The makeCacheMatrix() function caches the inverse of the matrix once the inverse is calculated.
##By calling the speicified functions on the operand matrix, various operations can be performed.
##Operations include :
##      1. setting the matrix, if it is not already supplied.
##      2. getting the matrix as output, for purposes including verification of entry
##      3. setting the inverse, once it is calculated; to be used as the cache
##      4. getting the inverse as output

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL                       ##inv stores the inverse and is initially NULL
        set<-function(y){               ##sets the matrix if it is not.
                x<<-y                   ##x is at another environment, hence the use of "<<-"
                inv<<-NULL              ##inv is still to be calculated
        }
        get<-function() x               ##view/store the input matrix
        setinverse<-function(inverse){  ##stores the inverse, once calulated
                inv <<- inverse
        }
        getinvese<-function() inv       ##returns the inverse
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinvese=getinvese)
}


## The function first checks whether the inverse is calculated and stored
##If it is, then the inverse matrix is returned
##If not, then it is first calculated using solve() and stored using setinverse() then returned


cacheSolve <- function(x, ...) {
        inv<-x$getinverse()             ##if inv=NULL, the computation is not done yet
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ##This section of code executes if inv=NULL
        ##hence computation is required.
        data<-x$get()                   ##input is stored in a temporary variable(preserved x)
        inv<-solve(data, ...)           ##... assumes the ... of the main func
        x$setinverse(inv)               ##inv is set, i.e. cached for future use
}

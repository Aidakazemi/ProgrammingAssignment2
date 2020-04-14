## Pair of functions that cache the inverse of a matrix

## Creates a special "matrix" that catche its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        #Initialize the inverse
        m <- NULL
        
        #setting the matrix
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        #getting and returning the matrix
        get <- function() x
        
        #setting and getting and returning the inverse of the matrix
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function(m)
                
        #retunr a list of the methods        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}





## Calculates the inverse of the special "matrix" created with the above function "makeCacheMatrix"
##If the inverse has been calculated, then the cashSolve gets the inverse from the cache and skip the computation.
##Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache viasetInverse function

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        #retunr the inverse if it has already been set
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ## get the inverse from the value of the matrix in the cach via setInverse function
        data <- x$get()
        m <- inverse(data,...)
        x$setInverse(m)
        
        ##return the matrix
        m
}

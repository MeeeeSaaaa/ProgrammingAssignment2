## function to cache the inverse of a matrix
## Creating matrix function to cache
makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL

   
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

   
    get <- function() {
    	
    	m
    }

    
    setInverse <- function(inverse) {
        i <<- inverse
    }

    
    getInverse <- function() {
      
        i
    }

   
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Creating a function to solve the matrix
cacheSolve <- function(x, ...) {

    ## get the matrix
    m <- x$getInverse()

    
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## get the solved data
    data <- x$get()

    
    m <- solve(data) %*% data

    ## set the inverse of matrix
    x$setInverse(m)

    
    m
}
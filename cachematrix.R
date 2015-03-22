## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computeting it repeatedly, 
## especially when working with larger datasets or computation looping. These two functions MakeCacheMatrix & cacheSolve have been written to cache the 
## inverse of a square matrix (makeCacheMatrix), and then use the cache (cacheSolve) -if available- instead of recalculating the inverse computation.

## makeCacheMatrix function: 
## This function uses a square invertable matrix as input. It is designed to cache the inverse of the input matrix. It returns a set of functions:  
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {                     # Initialize function. Input is a invertible square matrix
        INV <- NULL                                             # Initalize variabele (otherwise it returns an error on first run)
        set <- function(y) {                                    # Set:this function resets the cached INV (inverse) to NULL and set the cached x value to the passed value from the input matrix
                x <<- y                                         # use `<<-` to assign a value to an object in an environment different from the current environment.
                INV <<- NULL      
        }
        get <- function() x                                     # Create a function 'get' to assign the matrix to it
        setinverse <- function(solve) INV <<- solve             # Create a function 'setinverse' to compute and cache the inverse of the matrix
        getinverse <- function() INV                            # Create a function 'getinverse' to get the inverse matrix from the cache 
        list(set = set, get = get,                              # Return the list of functions of the makeCacheMatrix (set,get,setinverse,getinverse)
             setinverse = setinverse,
             getinverse = getinverse)
}

## Computes the inverse of the "matrix" returned by makeCacheMatrix(). If the inverse has already been calculated and the matrix has not changed, it retrieves the 
## inverse from the cache directly. When cache is not available it will calculate the inverse, and caches it for the next time. 

cacheSolve <- function(x, ...) {                                # Initialize function. Input is the output of makeCacheMatrix().
        INV <- x$getinverse()                                   # use getinverse get the inverse matrix from the x environment cache
        if(!is.null(INV)) {                                     # if the inverse has already been calculated (is not null), return it and print message "getting matrix-data from cache"
                message("getting matrix-data from cache")
                return(INV)
        }
        matrixdata <- x$get()                                   # Otherwise, set the matric into a local variable 
        INV <- solve(matrixdata, ...)                           # calculate the inverse
        x$setinverse(INV)                                       # put it in the cache using the setinverse function
        INV
}

## Testing matrix
## use: matrix <- matrix( c(1, 10, 3, 4, 5,6,7,8), nrow=2, ncol=2) 
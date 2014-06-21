## The three functions create overall fucntionality where a sample matrix is first created
## In makeCacheMatrix a special vector is created which is list of 
## set a matrix using function parameter
## get a matrix value
## set a matrix inverse using function parameter
## get a matrix inverse value
## In cacheSolve it is first checked in the inverse vaue exists in cache else the inversion operation is performed
## main function drive the above 2 functions by creating sampel matrix and calling the functions

## makeCacheMatrix returns a list of getters and setters for the matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(newmat) {
                x <<- newmat
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Checks if the inverse is in cache. If not calls the inverse
## Performs the actual inverse by calling the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        newinv <- x$getinv()
        if(!is.null(newinv)) {
                message("getting cached data")
                return(newinv)
        }
        data <- x$get()
        newinv <- solve(data)
        x$setinv(newinv)
        newinv
}

## Main function that creates a sample matrix, calls makeCacheMatrix and cacheSolve
mainFunction <- function() {
        ## Run mainFunction at the command line
        ## Provide the sample invertible matrix below
        samplematrix <- matrix(c(4,3,3,2),2,2)
        newlist <- makeCacheMatrix(samplematrix)
        invertedmatrix <- cacheSolve(newlist)
        invertedmatrix
        
}

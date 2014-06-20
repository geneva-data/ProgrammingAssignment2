## Coursera Data Science - R Programming Assignment 2
##*****************************************************
##
## Methodology I used to complete this assignment was to take the CacheMean example provided in the assignment description and
## change the vector type to matrix and sustitute the references to mean command with the inverse command
##
##***************************************************
##
## Results of the execution of this code were pasted below the code as comments
##
##****************************************************
## Caching the Inverse of a matrix in order to save computation time
## The first function, makecachematrix creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the inverse value of the matrix
## get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(inverse) m <<- inverse
    getmean <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = setinverse)
}


## The following function calculates the inverse of the special matrix "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse  of the data and sets the inverse value of the matrix row/colum 
## in the cache via the setinversefunction.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- inverse(data, ...)
    x$setinverse(m)
    m
}

## to test this:
## 1) created a matrix x<-matrix(c(2,4,1,3,2,4,5,1,2),3,3)
## 2) Solve(x)

##           [,1]        [,2]       [,3]
##[1,]  0.0000000  0.28571429 -0.1428571
##[2,] -0.1428571 -0.02040816  0.3673469
##[3,]  0.2857143 -0.10204082 -0.1632653
##> x
##[,1] [,2] [,3]
##[1,]    2    3    5
##[2,]    4    2    1
##[3,]    1    4    2

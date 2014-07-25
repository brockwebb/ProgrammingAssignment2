### JHU Coursera R Programming Course rprog-005
## Programming assignment #2 Submission
## This was modified code from the vector examples for the matrix problem

## These are a pair of functions built to cache the inverse of a matrix.
## The reasoning given is that this is an expensive operation, so cacheing 
## it can have an advantage in speeding up processing time...

## Test commands and outputs follow below the the following two functions:
## makeCacheMatric and cacheSolve ...

## Function:    makeCacheMatrix
## Description: Creates a list that sets/gets a matrix value, then sets/gets 
## the inverse of the matrix... 

makeCacheMatrix <- function(x = matrix()) {

        my_matrix <- NULL
        
        set <- function(y) {
                x <<- y
                my_matrix <<- NULL
        }

        get <- function() x

        setmatrix <- function(solve) my_matrix <<- solve

        getmatrix <- function() my_matrix
 
        list(set = set, get = get,
                setmatrix = setmatrix,
                getmatrix = getmatrix)
        

}

## Function:    cacheSolve
## Description:  Returns a matrix that is the inverse of 'x'
## Please note: use of fcn solve(A) works for a square matrix only*
##   * -- reference: http://www.statmethods.net/advstats/matrix.html

cacheSolve <- function(x, ...) {

        ## First check to see if the get_matrix function returned something
        ## If it is not null, return the matrix
        ## else, compute the inverse and set the cache for future calls
        
        my_matrix <- x$getmatrix()

        if(!is.null(my_matrix)) {
        message("getting cached data")
                return(my_matrix)
        }

        matrix <- x$get()
        my_matrix <- solve(matrix, ...)
        x$setmatrix(my_matrix)
        my_matrix

}

##Test this with the following commands:
##      > x = rbind(c(1, 0,4), c(1, 3,4), c(4,1,0))  
##      > cx=makeCacheMatrix(x)
##      > cx$get()
##              [,1] [,2] [,3]
##              [1,]    1    0    4
##              [2,]    1    3    4
##              [3,]    4    1    0
##      > cacheSolve(cx)
##              [,1]        [,2]    [,3]
##              [1,]  0.08333333 -0.08333333  0.2500
##              [2,] -0.33333333  0.33333333  0.0000
##              [3,]  0.22916667  0.02083333 -0.0625
##      > cacheSolve(cx)
##              getting cached data
##              [,1]        [,2]    [,3]
##              [1,]  0.08333333 -0.08333333  0.2500
##              [2,] -0.33333333  0.33333333  0.0000
##              [3,]  0.22916667  0.02083333 -0.0625        
## Remember that the inverse of a matrix times itself =1 ... lots of 
## floating point make it not perfectly equal to this matrix:
## Matrix*Matrix Inverse = rbind(c(1,0,0), c(0,1,0), c(0,0,1))
##      > x%*%cacheSolve(cx)
##              getting cached data
##              [,1]         [,2] [,3]
##              [1,] 1.000000e+00 1.387779e-17    0
##              [2,] 1.110223e-16 1.000000e+00    0
##              [3,] 0.000000e+00 0.000000e+00    1

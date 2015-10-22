## This function creates a special "matrix" object that can cache its inverse.
## example: 
## > a <- makeCacheMatrix(matrix(c(1,3,2,4),2,2))
## > cacheSolve(a)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
## > cacheSolve(a)
## getting inverse from the cache
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

makeCacheMatrix <- function(x = matrix()) {
        ## create cache environment 
        ## assign null value
        m <- NULL
        ## create matrix in current environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get value of the matrix
        get <- function() x
        ## set the value of the inverted matrix
        ## store in cache
        set_inv_matrix <- function(solve) m <<- solve
        ## get value of the inverted matrix from cache 
        get_inv_matrix <- function() m
        ## return to working environment
        list(set = set, get = get,
             set_inv_matrix = set_inv_matrix,
             get_inv_matrix = get_inv_matrix)
             
}
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 

## Computing the inverse of a square matrix can be done with 
## the solve function in R. For example, if X is a square invertible matrix, 
## then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return inverse matrix from cache
        m <- x$get_inv_matrix()
        ## If the inverse has already been calculated 
        ## (and the matrix has not changed), 
        ## then cacheSolve should retrieve the inverse from the cache
        ## to the current environment
        if(!is.null(m)) {
                message("getting inverse from the cache")
                return(m)
        }
        ## create matrix
        matrix <- x$get()
        ## compute inverse of matrix
        m <- solve(matrix, ...)
        ## cache inverted matrix
        x$set_inv_matrix(m)
        ## print cache
        m
}

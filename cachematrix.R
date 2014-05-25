## the first function stores a matrix and caches its inverse as calculated and passed to by the second function
## the second function computes newly the inverse, passes it to function 1 and displays it, or (if matrix has not changed and inverse has been calculated before) returns the inverse of a matrix cached in the first function




## makeCacheMatrix creates the variable x in the function. x can take a random matrix argument
##if no arguments are passed to x, m is set to NULL
##if a matrix argument is passed, "set" stores it in variable x (parent and subfunction environment), and sets m in the parent environment and the subfunction to NULL
##"get" returns x
##"setinverse" stores (cache) the local variable "solve" in setinverse and assigns it to m (parent and subfunction environment) such that a value associated with solve is assigned to m
##"getinvere" calls the value associated with m
##the four subfunctions are grouped in a single list object that can be called by their names

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
    
}


## m is called as a variable associated with and stored in the "getinverse" and "setinverse" subfunctions, respectively in function 1
## if a value is returned (not NULL) a message is displayed (getting cached data) and the value stored in m (=x$getinverse) is displayed.
## if m = NULL, a variable (data) is created that calls the matrix stored in the "get" subfunction of  function 1
## the variable m is then created that is associated with the inverse of the matrix data (=x$get) calculated by the function solve()
## the calculated inverse matrix is then passed to the "setinverse" function in function 1 and stored there
## the inverse matrix is returned as given by the local variable m

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


## testing the functions with a random 2:2 matrix 
testmatrix = makeCacheMatrix(matrix(rnorm(4),2,2))   #creates a matrix
testmatrix$get()                                # Returns original matrix as stored in function 1
testmatrix$getinverse()                         # check that no inverse has been calculated and stored in function
cacheSolve(testmatrix)                          # Computes, caches, and returns matrix inverse
testmatrix$getinverse()                         # Returns matrix inverse stored in function 1
cacheSolve(testmatrix)                          # Returns inverse of cached matrix using previously computed matrix inverse and displaying message
testmatrix$getinverse()%*%testmatrix$get()      # verifies that original matrix and inverse matrix return the identity matrix when multiplied

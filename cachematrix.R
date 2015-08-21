makeCacheMatrix <- function(n = matrix()) {
    ninv <- NULL
    set <- function(y) {                        #function to assign value to the matrix and reset the inverse
        n <<- y                                 #matrix object
        ninv <<- NULL                           #inverse of matrix n
    }
    get <- function() n                         #function to retrieve the matrix
    setninv <- function(inverse) ninv <<- inverse
    getninv <- function() ninv                  #function to retrieve the inverse of matrix n
    list(set = set, get = get,                  #storing a list of functions
    setninv = setninv,
    getninv = getninv)
}

cacheSolve <- function(n, ...) {
    ninv <- n$getninv()                         #retrieving the inverse
    if(!is.null(ninv)) {
        message("Retrieving Cached Data . . .")
        return(ninv)                            #printing cached inverse
    }
    myData <- n$get()                           #retrieving the new matrix
    ninv <- solve(myData, ...)                  #calculating its inverse
    n$setninv(ninv)                             #assigning new value to the inverse of n variable
    ninv                                        #printing the inverse
}
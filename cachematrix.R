## The first function creates an object, which takes matrix as an argument and sets several
## methods for this object to operate with matrix, such as get&set and setInv&getInv.
## Moreover, this object saves the inverse matrix, which is set but corresponding setInv and getInv
## functions.

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    get <- function() x
    setInv <- function(solve) invM <<- solve
    getInv <- function() invM
    list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}

## The second function takes the structure and checks if there is already set inverse of the cached
## matrix. If the value is already calculated, the function returns the saved value, if not
## it calculates the value, saves it in the cached matrix function and then returns the calculated
## value.

cacheSolve <- function(x, ...) {
    invM <- x$getInv()
    if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
    data <- x$get()
    invM <- solve(data, ...)
    x$setInv(invM)
    invM 
}

# Example of execution: 
#> mm <- makeCacheMatrix(matrix(rnorm(4), 2,2))
#> cacheSolve(mm)
#[,1]       [,2]
#[1,] 0.8263517 -0.6953132
#[2,] 0.7798220 -2.9974098
#> cacheSolve(mm)
#getting cached data
#[,1]       [,2]
#[1,] 0.8263517 -0.6953132
#[2,] 0.7798220 -2.9974098
# The value is also checked with a direct execution for the given matrix:
#solve(mm$get())
#[,1]       [,2]
#[1,] 0.8263517 -0.6953132
#[2,] 0.7798220 -2.9974098
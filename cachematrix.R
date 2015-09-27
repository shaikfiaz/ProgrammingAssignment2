## The following set of two functions enable the programmer to generate a special matrix along with its inverse. 
## To save time, if the inverse is already calculated it displays the message saying "Inver already calculated and getting cached data"

## Function "makecacheMatrix" creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makecacheMatrix <- function(x = matrix()) {
        I <- NULL #For inverse
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInv <- function(solve) I <<- solve
        getInv <- function() I
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## The function "cacheSolve" calculates the inverse of the special "matrix" created with the "makecacheMatrix" function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        I <- x$getInv()
        if(!is.null(I)) {
                message("Inver already calculated and getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInv(I)
        I
}

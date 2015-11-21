## The following set of two functions enable the programmer to generate a special matrix along with its inverse. 
## To save time, if the inverse is already calculated it displays the message saying "Inverse already calculated and getting cached data"

## Function "makecacheMatrix" creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makecacheMatrix <- function(x = matrix()) {
        I <- NULL
        #Function to set the value of the matrix
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        # Function to get the matrix
        get <- function() x 
        # Function to calculate the inverse of the matrix
        setInv <- function(solve) I <<- solve
        # Function to get the inverse of the matrix
        getInv <- function() I
        # Output of makecacheMatrix containing functions for setting, getting, calculating inverse, getiing the inverse of matrix
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## The function "cacheSolve" calculates the inverse of the special "matrix" created with the "makecacheMatrix" function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        I <- x$getInv()#captures the inverse of the matrix from the list "x"
        # Checking if the inverse already exist in the list and returning its value as output
        if(!is.null(I)) {
                message("Inverse already calculated and getting cached data")
                return(I)
        }
        #We reach this part of the function if the inverse is not already there in the list "x"
        data <- x$get() #To get the matrix
        I <- solve(data, ...) #Calculating the inverse
        x$setInv(I) #Setting the value of inverse in the list "x"
        I #returning inverse as out of the function cacheSolve
}

## Using makeCacheMatrix and cacheSolve it is possible to calculate the inverse of a matrix assuring
## that the calculation is performed only the first time it is called for a certain matrix

## Example of use:
## sample<-matrix(c(1,3,99,4,3,2,7,6,9,15,43,12,27,81,1,25),4,4)
## vsample<-makeCacheMatrix(sample)
## cacheSolve(vsample) ## the first time the inverse is calculated
## cacheSolve(vsample) ## the second time the inverse is retrieved from the cache

## makeCacheMatrix(x) creates a list containing 4 functions referenced to a matrix x in order to
## set or get the value of the matrix, and set or get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {

## makeCacheMatrix(x) creates a list containing 4 functions referenced to a matrix x in order to
## set or get the value of the matrix, and set or get the value of its inverse

## The inverse matrix (inv) is initialized with a NULL value

        inv <- NULL
## 1. function to set the value of the matrix
        set <- function(y) {

	## With the <-- operator we assign the value on a higher environment so that the states of 
	## the objects are preserved

                x <<- y
                inv <<- NULL
        }
## 2. function to get the value of the matrix
        get <- function() x
## 3. function to set the value of the inverse
        setinverse <- function(inverse) inv <<- inverse
## 4. function to get the value of the inverse
        getinverse <- function() inv

## A list containing the four functions as a result of makeCacheMatrix is retrieved
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve restores the inverse of a matrix but calculating it only the first time is called
## the input of this function is a list created with the function makeCacheMatrix

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
## the input of this function is a list created with the function makeCacheMatrix

## the inverse matrix is stored in 'inv' variable and can be queried with the 'getinverse' function

        inv <- x$getinverse()

## if 'inv' is not null it means that the value inverse has been previously cached in this variable,
## so it can be queried and returned instead of calculating it

        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

## if 'inv' is null the value of the inversed matrix is not scored, so it must be calculated 
## (with solve), stored (with setinverse) and returned

        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}

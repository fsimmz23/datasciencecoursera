## The purpose to create the two function is to cache the inverse of a matrix
## This way we avoid recomputing the inverse matrix if its cached. If we don'that
## cache it usually will be costly computation.

## Write a short comment describing this function
## The makeCacheMatrix:
## To be able to cache. Create a special
## matrix that will help us with this by using the
## makeCacheMatrix function.  The input into this function
## is simply a variable of type matrix.

makeCacheMatrix <- function(x = matrix()) {
	## Following the same format as the assignment example
	## Creating a makeCacheMatrix object will consist of
	## four functions encapsulated in a list
	## 1. set the matrix
	## 2. get the matrix
	## 3. set the inverse of the matrix
	## 4. get the inverse of the matrix

	## Initially we set the inv to NULL
	## the value Changes when the user sets the value
    inv <- NULL

    ## set the y function
    ## Sets the matrix itself but not the inverse
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }

    ## get the function
    ## we gets the matrix itself. Note not the inverse
    get <- function() x

    ## We manually set the inverse
    setinverse <- function(inverse) inv <<- inverse

    ## We get the inverse
    getinverse <- function() inv

    ## Encapsulate into a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


	## cacheSolve:
	## after you created the matrix, use the cacheSolve function to compute the inverse and cache the result
	
	
	## If you use the cacheSolve again on the same matrix, then the pre-computed result will be retrived, hence
	## avoiding any recomputation.  An informative message
	## will be shown in the command prompt when the pre-computed
	## result is returned instead.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Changing the assignment example
	## Changing the mean to inverse

    ## Get the current state of the inverse and see if it
    ## has been computed yet
    inv <- x$getinverse()

    ## If it has...
    if(!is.null(inv)) {
	
    ## Return the computed inverse		
        message("Getting cached matrix")
        return(inv)
    }

    
    ## Get the matrix itself
    data <- x$get()

    ## Find the inverse
    inv <- solve(data, ...)

    ## The result is cached in the object
    x$setinverse(inv)

    ## Return the new result
    inv  
}

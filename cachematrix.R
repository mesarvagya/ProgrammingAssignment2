## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	#Initialization Step for getting value to i
	i <<- NULL

	#Method for setting matrix
	set <- function(y) {
                    x <<- y
                    i <<- NULL
    }

    #Method for getting matrix
    get <- function(){
    	x
    }
    #Method for setting the Inverse of Matrix
    setInverse <- function(inverse){
    	i <<- inverse
    }
    #Method for getting the Inverse of Matrix
    getInverse <- function(){
    	i
    }
    #Return List of Methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function

# Calculate the inverse of the matrix returned by 'makeCacheMatrix'
# above. If the inverse has been already been calculated (given that matrix hasn't changed)
# then the "cachesolve" does retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()


        #Check if inverse is already calculated. If so, return cached data
        if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
        }

        #Get Matrix from the object
        data <- x$get()

        #Calculate the Inverse using matrix Multiplication
        m <- solve(data) %*% data

        #Set the inverse to object
        x$setInverse(m)

        #Return the matrix
        m

}

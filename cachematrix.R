## Below we have two functions that create a special 'matrix' and compute and cache the inverse of a special 'matrix'

## The function makeCacheMatrix creates a special 'matrix' which is really a list containing: 1. the function 'set' to set the value of the matrix, 2. the function 'get' to get the value of the matrix, 3. the function 'setinverse' to set the inverse of the matrix and 4. the function 'getinverse' to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <<- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}	
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of a special 'matrix' created with the function above. First it checks if the inverse has already been computation by getting the inverse from the cache using the getinverse function and then checking if the cached inverse is not equal to NULL. If it finds that the cached inverse is not NULL it returns it. Else, it computes the inverse using solve(), then sets the inverse of the 'matrix' in the cache using the setinverse function and then returns the inverse 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        mat <-x$get()
        inv <- solve(mat,...)
        x$setinverse(inv)
        inv
}

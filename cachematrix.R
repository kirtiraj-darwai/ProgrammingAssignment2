## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly.  
## The following function will calculate the inverse matrix and return it from cache, 
## if its available.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x_matrix = numeric()) {
        m_inverse <- NULL
        set <- function(y_matrix) {
                x_matrix <<- y_matrix
                m_inverse <<- NULL
        }

        get <- function() x_matrix
        setinverse <- function(inverse)	m_inverse <<- solve(inverse)
        getinverse <- function() m_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x_matrix, ...) {
        m_inverse <- x_matrix$getinverse()
        if(length(m_inverse)!=0) {
                message("getting cached data")
                return(m_inverse)
        }
        data <- x_matrix$get()
	m_inverse <- solve(data)
        x_matrix$setinverse(m_inverse)
        m_inverse
}

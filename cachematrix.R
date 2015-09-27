## Inverting a large matrix is computationally costly
## Following two functions will provide a way to cache an inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
        
        # Creates special matrix, which is a list containing functions to
        # 1. set the value of the matrix (set)
        # 2. get the value of the matrix (get)
        # 3. set the value of inverse of the matrix (setinverse)
        # 4. get the value of inverse of the matrix (getinverse)
        # Args:
        #   x: The matrix
        #
        # Returns:
        #   The special matrix (i.e. The function list)
        
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
        
        # Returns the inverse of the special matrix
        # It first check if the inverse is already calculated. 
        # Then it will return the calculated value. 
        # If not, it will calculate the value and cache it 
        # using setinverse function
        #
        # Args:
        #   x: The special matrix
        #
        # Returns:
        #   A matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data) # Assume 'data' is a square matrix
        x$setinverse(inv)
        inv
}

# Test Case
# > x = rbind(c(2, 5), c(5, 2))
# > mat = makeCacheMatrix(x)

# > mat$get()
# [,1] [,2]
# [1,]    2    5
# [2,]    5    2

# > cacheSolve(mat)
# [,1]       [,2]
# [1,] -0.0952381  0.2380952
# [2,]  0.2380952 -0.0952381

# > cacheSolve(mat)
# getting cached data.
# [,1]       [,2]
# [1,] -0.0952381  0.2380952
# [2,]  0.2380952 -0.0952381

# > solve(x)
# [,1]       [,2]
# [1,] -0.0952381  0.2380952
# [2,]  0.2380952 -0.0952381
# > 

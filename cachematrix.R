rm(list = ls())
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The makeCacheMatrix create a special matrix object this is able to store
# the its own inverse (the inverse of the matrix itself). When the inverse is 
# already known, the special matrix returns it without computing it again

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

# Computes the inverse of a CacheMatrix, it the inverse is already cached
# is return the reverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached inverse...")
                return(inv)
        }
        # Inverse is not yet cached, computing it right now
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

# # Extra code for testing the functions
# # =============================================================================
# 
# # 1. Create random 5x5-matrix
# m <- matrix(rnorm(25), nrow = 5, ncol = 5) 
# m
# 
# # 2. Compute the inverse to compare it later on with the cacheSolve-result
# solve(m)        
# 
# # 3. Make a Cache Matrix
# mc <- makeCacheMatrix()
# 
# # 4. Store m in mc and check it
# mc$set(m)
# mc$get()
# 
# # 5. First time: calculates the inverse
# cacheSolve(mc)  
# 
# # 6. Second time: gets inverse from cache
# cacheSolve(mc)  


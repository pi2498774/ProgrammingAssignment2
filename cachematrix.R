## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The makeCacheMatrix create a special matrix object this is able to store
# the its own inverse (the inverse of the matrix itself). When the inverse is 
# already known, the special matrix returns it without computing it again

makeCacheMatrix <- function(x = matrix()) {
        invX <- NULL
        set <- function(y) {
                x <<- y
                invX <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) invX <<- inverse
        getinv <- function() invX
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

# Computes the inverse of a CacheMatrix, it the inverse is already cached
# is return the reverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invX <- x$getinv()
        if(!is.null(invX)) {
                message("Getting cached inverse...")
                return(invX)
        }
        # Inverse is not yet cached, computing it right now
        orig_matrix <- x$get()
        invX <- solve(orig_matrix)
        x$setinv(invX)
        invX
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


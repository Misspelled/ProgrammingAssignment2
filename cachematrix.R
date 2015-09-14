# This function initializes an object (a list of functions) that allows us to track whether
# the inverse of a given matrix has been computed. If it has,
# the object stores the inverse so it can be retrieved without being 
# recomputed. 

initializeMatrix <- function(mtrx) {
    inv <- NULL
    list(get = function() {mtrx},      # return the matrix currently assigned to object
         set = function(new_mtrx) {    # function to assign different matrix to object
             mtrx <<- new_mtrx      
             inv <<- NULL              # inverse is set to NULL so it will be computed, not retrieved
         },
         get_inv = function() {inv},   # return stored inverse (or NULL if inverse hasn't been computed)
         set_inv = function(new_inv) { # update value of inverse
             inv <<- new_inv
         }
    )
}

# This function uses the object returned by initializeMatrix() to test if 
# the inverse of a matrix has been computed. It retrieves the stored inverse
# if available. If not, it computes the the inverse and then stores it via a call to 
# $set_inv 

getCachedInverse <- function(m,...) {   # m is return value from initializeMatrix()
    tst <- m$get_inv()                  # test if inverse has been computed
    if(!is.null(tst)) {                 # and return it if it has
        return(tst)                     
    }
    inv <- solve(m$get())               
    m$set_inv(inv)                      # store computed inverse
    inv    
}

# construct a matrix and initialize first function for testing
vec <- rpois(1000000, 50)
test_matrix <- matrix(vec, nrow = 1000, ncol = 1000)
rm(vec)
cache_store <- initializeMatrix(test_matrix)

# compute system time to confirm inverse is being retrieved
# and calculated appropriately

inverse1 <- system.time(getCachedInverse(cache_store))
inverse2 <- system.time(getCachedInverse(cache_store))

#cache_store$set_inv(test_matrix)
#cache_store$get_inv()
#system.time(solve(test_matrix))

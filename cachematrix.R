# This function initializes an object (a list of functions) that allows us to track whether
# the inverse of a given matrix has been computed. If it has,
# the object stores the inverse so it can be retrieved without being 
# recomputed. 

makeMatrix <- function(mtrx) {
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

# This function uses the object returned by makeMatrix() to test if 
# the inverse of a matrix has been computed. It retrieves the stored inverse
# if available. If not, it computes the the inverse and then stores it via a call to 
# makeMatrix$set_inv() 

cacheinverse <- function(m,...) {   # m is return value from makeMatrix()
    tst <- m$get_inv()                  # test if inverse has been computed
    if(!is.null(tst)) {                 # and return it if it has
        return(tst)                     
    }
    inv <- solve(m$get())               
    m$set_inv(inv)                      # store computed inverse
    inv    
}

# construct a matrix and call makeMatrix()  for testing
vec <- rpois(1000000, 50)
test_matrix <- matrix(vec, nrow = 1000, ncol = 1000)
rm(vec)
cache_store <- makeMatrix(test_matrix)

# compute system time to confirm inverse is being retrieved
# and calculated appropriately

inverse1 <- system.time(cacheinverse(cache_store))
inverse2 <- system.time(cacheinverse(cache_store))
print(inverse1) # system time for calculation
print(inverse2) # system time for retrieval
inverse1['user.self'] > inverse2['user.self'] # should return TRUE


# test that inverse is reset to NULL when matrix
# is changed in makeMatrix()

vec <- runif(1000000)
new_test_matrix <- matrix(vec, nrow = 1000, ncol = 1000)
cache_store$set(new_test_matrix)
is.null(cache_store$get_inv()) # should return TRUE


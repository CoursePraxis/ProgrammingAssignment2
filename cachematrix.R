
############################################################################################# 
#	Assignment: Caching the Inverse of a Matrix
#############################################################################################  
#	The assignment is to write a pair of functions that cache the inverse of a matrix.
##############################################################################################

# 1.	makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
####
# 2.	cacheSolve: This function computes the inverse of the special "matrix" returned by 
#     makeCacheMatrix above. 
#	If the inverse has already been calculated (and the matrix has not changed), 
#	then the cachesolve should retrieve the inverse from the cache.

# 	Computing the inverse of a square matrix can be done with the solve function in R. 
#	For example, if X is a square invertible matrix, then solve(X) returns its inverse.
#######################################################################


## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache

############################################

makeCacheMatrix <- function(x = matrix()) {
        
        # initializes the matrix in Cache
        cacheMat <- NULL

        # creates the matrix in the main function
        set <- function(y) {
                x <<- y
                cacheMat <<- NULL
        }

        # get the value of the matrix stored in the main function
        get <- function() x

        # invert the matrix and store in cache
        setMatInv <- function(inverse) cacheMat <<- inverse

        # get the inverted matrix from cache
        getMatInv <- function() cacheMat

        # To store  the created functions in the main function
        list(set = set, get = get,
             setMatInv = setMatInv,
             getMatInv = getMatInv)
}

############################################
## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve  retrieves the inverse from the cache.

## If the inverse does not exist in cache,
## it it is created in the main function and the inverse
## is now stored in cache
############################################

cacheSolve <- function(x, ...) {
        ## try to get the inverse of the matrix from the cache
        cacheMat <- x$getMatInv()

        # retrieve  matrix Inverse from cache if it exists
       
        if (!is.null(cacheMat)) {
                message("getting matrix inverse from cache")

                # display matrix 
                return(cacheMat)
        }
 	  # else create the matrix in the main function
        # create matrix if it does not exist
        matrix <- x$get()

        # Use solve to find inverse if  matrix is square and invertible
        # else show error message or warnings
        tryCatch( {
                # use solve to get  inverse of a square matrix
                cacheMat <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)

                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)

                return(NA)
        },
        finally = {
                # store  matrix inverse in cache
                x$setMatInv(cacheMat)
        } )

        # display matrix inverse
        return(cacheMat)
}

######################End of Code

## +-------------------------------------------------------------------------------------+
## | Cache Matrix functionality implemented with "first-class functions"                 |
## | Created by: Marcelo Gomes Marques - At: 04/25/2014        				 |
## | Created with R x64 3.1.0 Patched on Windows 7 64 bits SO	        		 |
## +-------------------------------------------------------------------------------------+
## | ATTENTION: At my system I need use the '@' instead the '$' symbol to call a function| 
## |            closure, or "first-class function", defined at makeCacheMatrix.          |
## |            If you have problem maybe you are on older version of R and need changes |
## |            the '@' symbol to '$' at calls makeCacheMatrix's functions closure in the|
## |            acheSolve function.							 |
## +-------------------------------------------------------------------------------------+

## Overall description: This function use a special 'matrix' object that can cache
## its inverse based on a formal class 'cacheSolve' specified in makeCacheMatrix function
##
## This Inverse matrix's cache constructor function named 'makeCacheMatrix' first checks 
## to see if the inverse matrix has already been calculated. If so, it gets the inverse  
## matrix from the cache and skips the computation. Otherwise, it calculates the inverse
## matrix of the data by original solve function and sets the value of the inverse matrix
## in the cache via the setSolve function.
##
## For you can use this new functionality, first you need create a new object of the 
## class 'cacheSolve' by assign the makeCacheMatrix function with a square matrix to a
## variable  (e.g.: x <- makeCacheMatrix(matrix(rnorm(1:25),5,5))), than you just need
## call the function cacheSolve(x) twice to see the results inverse matrix and if need
## made a cache generation or just its use.
##
## e.g: at the prompt line command
## > a <- makeCacheMatrix(matrix(1:4,2,2))
## > cacheSolve(a)
## ******** New inverse matrix computed and cached the data ********
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(a)
## ******** getting cached data ********
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## *********************************** End Overall description **************************

## makeCacheMatrix function Description:
# This function take care of square matrix's cache constructor, check and getting functions
# using a function closure definitions to set variable data, with new matrix, to the get
# to assign the values newvalue of a new matrix informed and NULL to dataSolve in an 
# environment that is different from this function, to save the result on cache of 
# makeCacheMatrix 'cacheSolve', get the computed inverse matrix if all ready computed and
# create a new object of formal class cacheSolve with the data of matrix, its inverse and
# functionalities when new matrix is submitted to makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {
        ## Inverse matrix's cache constructor, check and getting functions
        data <- x					# Initialize variables data whit square matrix
        dataSolve <- NULL				# and dataSolve with null
        get <- function() data				# Definition of get function closure, 
                                                        # to set variable data, with new matrix, to the get
        set <- function(newvalue) {			# Definition of set function closure, 
                data <<- newvalue			# to assign the values newvalue of a new matrix informed 
                dataSolve <<- NULL			# and NULL to dataSolve in an environment that is different 
                                                        # from this function
        }
        setSolve <- function(newSolve) 			# Definition of setSolve function closure to 
                dataSolve <<- newSolve			# save the result on cache of makeCacheMatrix 'cacheSolve'
        getSolve <- function() dataSolve		# Definition of getSolve function closure to 
                                                        # get the computed inverse matrix all ready computed
        new("cacheSolve", set = set, get = get,         # Create a new object of formal class cacheSolve with
            setSolve = setSolve,			# the data of matrix its inverse and functionalities 
            getSolve = getSolve)
}

# cacheSolve function definition:
# This function initiate with try get the computed inverse matrix 'a' at list's cacheSolve 
# object and assign to InvMat variable. If there is a cache for the inverse matrix a 
# message seed to user inform that has a cache and return the value of cache with no new
# inverse matrix computation needed. Otherwise, the new matrix is assign to get on 
# makeCacheMatrix and a new compute inverse matrix computed with solve function. 
# Finally setSolve function is called for save the result on cache, send a message to user
# to inform that a new computation was performed and presents the results of inverse matrix.
## +-------------------------------------------------------------------------------------+
## | ATTENTION: At my system I need use the '@' instead the '$' symbol to call a function| 
## |            closure, or "first-class function", defined at makeCacheMatrix. 	 |
## |            If you have problem maybe you are on older version of R and need changes |
## |            the '@' symbol to '$' at calls makeCacheMatrix's functions closure in the|
## |            acheSolve function.							 |
## +-------------------------------------------------------------------------------------+

cacheSolve <- function(a, ...) {
        InvMat <- a@getSolve()      # Maybe you need to cange this '@' symbol to '$' 
        if(!is.null(InvMat)) {  
                message("******** Getting cached data ********");  
                return(InvMat);
        }
        data <- a@get()             # Maybe you need to cange this '@' symbol to '$' 
        InvMat <- solve(data, ...)
        a@setSolve(InvMat)          # Maybe you need to cange this '@' symbol to '$' 
        message("******** New inverse matrix computed and cached the data ********") 
        InvMat 
}
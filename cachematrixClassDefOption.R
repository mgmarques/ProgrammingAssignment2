## +-------------------------------------------------------------------------------------+
## | Cache Matrix functionality implemented with formal class definitions and setMethod  |
## | Created by: Marcelo Gomes Marques - At: 04/25/2014        				 |
## | Created with R x64 3.1.0 Patched on Windows 7 64 bits SO				 |
## +-------------------------------------------------------------------------------------+
## | ATTENTION: At my system I need use the '@' instead the '$' symbol to call a function| 
## |            closure, or "first-class function", at makeCacheMatrix. 		 |
## |            If you have problem maybe you are on older version of R and need change  |
## |            the '@' symbol for '$' at the setMethod code parts.			 |
## +-------------------------------------------------------------------------------------+
## 
## Overall description: This function use a special 'matrix' object that can cache
## its inverse based on a new class definition named by 'cacheSolve' and used by a
## Inverse matrix's cache constructor function named 'makeCacheMatrix
##
## it first checks to see if the inverse matrix has already been calculated. If so, 
## it gets the inverse matrix from the cache and skips the computation. Otherwise, 
## it calculates the inverse matrix of the data by original solve function and sets
## the value of the inverse matrix in the cache via the setSolve function.
##
## For you can use this new functionality, first you need create a new object of the 
## class 'cacheSolve' by assign the makeCacheMatrix function with a square matrix to
## a variable  (e.g.: x <- makeCacheMatrix(matrix(rnorm(1:25),5,5))), than you just 
## need call the function solve(x) twice to see the results of cache generation and 
## use.
##
## You can see the new object created by using the show method "for the e.g. above 
## typing at the command line '> show(x)'"
## 
## e.g:
## > set.seed(23)
## > y <- makeCacheMatrix(matrix(rnorm(1:36),6,6))
## > print(solve(y))
##            [,1]       [,2]       [,3]        [,4]       [,5]       [,6]
## [1,]  0.7406234  1.9035288  0.0838657  0.01333696 -0.2261180  1.6335784
## [2,] -2.6396702 -6.4304336 -1.3230133  0.66662165  3.1741883 -4.9082468
## [3,]  3.9903287  8.1694332  0.7011361 -0.45656231 -3.2182772  5.5674936
## [4,]  1.0395226  3.7978118  0.9008047 -0.36640087 -1.5788347  2.5805103
## [5,] -0.8589856  0.3077144  0.5810149  0.09820942 -0.9639958  0.4999576
## [6,] -2.6063409 -4.7755155 -1.2049279  0.21051879  2.6402808 -3.1428642
## > print(solve(y))
## ************** Get Matrix cacheSolve **************
## [,1]       [,2]       [,3]        [,4]       [,5]       [,6]
## [1,]  0.7406234  1.9035288  0.0838657  0.01333696 -0.2261180  1.6335784
## [2,] -2.6396702 -6.4304336 -1.3230133  0.66662165  3.1741883 -4.9082468
## [3,]  3.9903287  8.1694332  0.7011361 -0.45656231 -3.2182772  5.5674936
## [4,]  1.0395226  3.7978118  0.9008047 -0.36640087 -1.5788347  2.5805103
## [5,] -0.8589856  0.3077144  0.5810149  0.09820942 -0.9639958  0.4999576
## [6,] -2.6063409 -4.7755155 -1.2049279  0.21051879  2.6402808 -3.1428642
## *********************************** End Overall description **************************

## Class cachesolve definition
setClass("cacheSolve",
         representation(set = "function",
                        get = "function",
                        getSolve = "function",
                        setSolve = "function"))

## makeCacheMatrix function Description:
# This function take care of square matrix's cache constructor, check and getting functions
# using a function closure definitions to set variable data, with new matrix, to the get
# to assign the values newvalue of a new matrix informed and NULL to dataSolve in an 
# environment that is different from this function, to save the result on cache of 
# makeCacheMatrix 'cacheSolve', get the computed inverse matrix if all ready computed and
# create a new object of formal class cacheSolve with the data of matrix, its inverse and
# functionalities when new matrix is submitted to makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {
        ## Inverse matrix's cache constructor function
        ## no differences from the Cache Matrix functionality implemented with "first-class functions"  
        data <- x
        dataSolve <- NULL
        get <- function() data
        set <- function(newvalue) {
                data <<- newvalue
                dataSolve <<- NULL
        }
        setSolve <- function(newSolve) dataSolve <<- newSolve
        getSolve <- function() dataSolve
        new("cacheSolve", set = set, get = get, setSolve = setSolve,
            getSolve = getSolve)
}


# Set a 'show' method for the new class cacheSolve
setMethod("show", "cacheSolve",
          function(object) {
                  ## Show the object content of new class cacheSolve
                  x <- object@get()
                  show(x)
                  invisible(x)
          })

# Set a new 'solve' method
# This method define a alternative function named solve and its is primarily used when
# a solve function is call. 
# It's initiate with try get the computed inverse matrix 'a' at list's cacheSolve 
# object and assign to InvMat variable. If there is a cache for the inverse matrix a 
# message seed to user inform that has a cache and return the value of cache with no new
# inverse matrix computation needed. Otherwise, the new matrix is assign to get on 
# makeCacheMatrix and a new compute inverse matrix computed with solve function. 
# Finally setSolve function is called for save the result on cache, send a message to user
# to inform that a new computation was performed and presents the results of inverse matrix.
#
# don't wary whit the message 'Creating a generic function for ‘solve’ from package ‘base’
# in the global environment'.  It is not a error and when you clear your environment your  
# solve function is normalized. 
# +-------------------------------------------------------------------------------------+
# | ATTENTION: At my system I need use the '@' instead the '$' symbol to call a function| 
# |            closure, or "first-class function", defined at makeCacheMatrix. 	        |
# |            If you have problem maybe you are on older version of R and need changes |
# |            the '@' symbol to '$' at calls makeCacheMatrix's functions closure in the|
# |            acheSolve function.							|
# +-------------------------------------------------------------------------------------+

setMethod("solve", "cacheSolve",
          function(a, b, ...) {
                  ## Return a matrix that is the inverse of 'x'
                  InvMat <- a@getSolve()      # Maybe you need to cange this '@' symbol to '$' 
                  if(!is.null(InvMat)) {
                          message("************** Get Matrix cacheSolve **************");
                          return(InvMat);}
                  data <- a@get()             # Maybe you need to cange this '@' symbol to '$' 
                  InvMat <- solve(data, ...)
                  a@setSolve(InvMat)          # Maybe you need to cange this '@' symbol to '$' 
                  message("******** New inverse matrix computed and cached the data ********") 
                  InvMat
          })

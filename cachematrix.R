## Overall description: This function use a special 'matrix' object that can cache
## its inverse based on a new class definition named by 'cacheSolve' and used by a
## Inverse matrix's cache constructor function named 'makeCacheMatrix'.
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

## Class cachesolve definition
setClass("cacheSolve",
         representation(set = "function",
                        get = "function",
                        getSolve = "function",
                        setSolve = "function"))

makeCacheMatrix <- function(x = matrix()) {
        ## Inverse matrix's cache constructor function
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


# A 'show' method for the new class cacheSolve
setMethod("show", "cacheSolve",
          function(object) {
                  ## Show the object content of new class cacheSolve
                  x <- object@get()
                  show(x)
                  invisible(x)
          })

# A new 'solve' method
setMethod("solve", "cacheSolve",
          function(a, b, ...) {
                  ## Return a matrix that is the inverse of 'x'
                  InvMat <- a@getSolve()
                  if(!is.null(InvMat)) {
                          message("************** Get Matrix cacheSolve **************");
                          return(InvMat);}
                  data <- a@get()
                  InvMat <- solve(data, ...)
                  a@setSolve(InvMat)
                  InvMat
          })
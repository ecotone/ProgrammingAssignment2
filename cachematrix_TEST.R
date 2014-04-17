## cachematrix_TEST.R
## "unit test" for the code in cachematrix.R


## mxEqual()
## Helper function, returns TRUE if the two matrices passed as
## its arguments are approximatively equal  (dealing with the
## imprecision that is implicit to Floating Point arithmetic)
mxEqual <- function (a, b, tolerance=.Machine$double.eps ^ 0.5) {
    isTRUE(all.equal(a, b, tolerance=tolerance))
}

## Functional test
##   Creates a random matrix of desired size and asserts that it works well
##   with the CacheMatrix system.
## Arguments:
##    n = nb of rows (and columns since matrix is square)
## Returns:  TRUE if all tests are successful
## 
CacheMatrix_FunctionalTest <- function(n) {
    retVal = TRUE
    
    A <- matrix(runif(n^2), n, n)
    cA <- makeCacheMatrix(A)
    
    # @@@ used to look into the private parts of the CacheMatrix object...
    #   assertions made off this variable are very implementation dependant, but useful.
    cA.env <- environment(cA$get) 
    
    # *** Basic functional round trip (twice to see with the cache)  ***
    invA <- cacheSolve(cA)
    if (!mxEqual(A %*% invA, diag(n))) {
        warning("First time call to cacheSolve() failed! ")
        return(FALSE)  # no point in continuing...
    }
#    if (!mxEqual(cA.env$x.inv, invA)) {
#        retval <- FALSE
#        warning("It appears cacheSolve() didn't cache the data... ?")
#    }
    invA2 <- cacheSolve(cA)
    if (!mxEqual(invA, invA2))  {
        retval <- FALSE
        warning("Second time call to cacheSolve() failed! ")
    }
    
    # *** Test of the get() and set()
    A <- matrix(runif(n^2), n, n)
    cA$set(A)
    
    if (!mxEqual(A, cA$get())) {
        retval <- FALSE
        warning("get() didn't return the matrix previously stored with set() !!!")
    }
    
    if (!mxEqual(A %*% cacheSolve(cA), diag(n))) {
        retval <- FALSE
        warning("Bad result from cacheResolve(), after having reused the CacheMatrix")
    }
   
    retVal
}

##  *** Test logic entry point is here ! ****

set.seed(54917)   # ensure reproducible results
cc <-CacheMatrix_FunctionalTest(2)

for (n in c(1,4,3, 4, 4, 7, 11)) {
    if (!CacheMatrix_FunctionalTest(n)) {
        error(paste0("CacheMatrix_FunctionalTest failed for n=", as.character(n)))
    }
}

## Check that makeCacheMatrix argument validity check works
##  The following should "fail" with a warning.
myM <- matrix(runif(15), 3, 5)
cMyM <- makeCacheMatrix(myM)              # not square
cMyM$set(matrix(rep(NA, 9), 3, 3))        # non invertible : contains NAs

myM <- matrix(runif(16), 4, 4)
myM[2,3] <- NaN
cMyM <- makeCacheMatrix(myM)              # not invertible : contains NAs


# finally a bit bit of fun with a singular matrix
# These unlike "not square" and "with NAs" cases above, non invertible matrixes
# are not detected during the makeCacheMatrix() or set() calls.
myM <- matrix(runif(9), 3, 3)
mySingM <- cbind(myM[, 1:2], myM[,1])
cachedMySingM <- makeCacheMatrix(mySingM)

noSuchM <- cacheSolve(cachedMySingM)     # should produce "Error in solve...." message





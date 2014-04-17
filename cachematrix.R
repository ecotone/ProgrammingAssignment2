## cachematrix.R
##   This file is an assignement for the 
##      'R Programming' coursera class (Instructor: Dr. Roger Peng)
###
##  !!!!!  Please stop reading if you are also taking the class and haven't
##  !!!!!  completed the assignment yet !
##
##  The code in this file introduces "CacheMatrix", a "wrapper" around 
##  matrix objects.
##  Such wrapped matrices can then be used in a function which memoizes the 
##  inverse of the matrix

    
## makeCacheMatrix()
##   Creates a CacheMatrix, i.e. a matrix wrapped in a list object. 
##   Such object can can then be used with the cacheSolve() function, which 
##   itself produces the inverse of the matrix by either computing it or by
##   returning a cached copy of the inverse.
##  Arguments :  x = a square matrix.   It is assumed to be nonsingular.
##  Return Value: a "CacheMatrix", i.e. a list object with the followng elements:
##        $get()  = function returning x, the underlying matrix
##        $set(x) = function to set the underlying matrix
##  --the following elements, marked with the p. prefix could/should be 
##    considered "protected" methods, to be used only by supporting  logic (e.g.
##    in cacheSolve()) but not by higher-level users of the "wrapped matrix".
##        $p.getInv()  = function to get the cached value of the matrix' inverse
##                      (returns null if this wasn't [yet] cached)
##        $p.setInv()  = function to set the cached value of the matrix
makeCacheMatrix <- function(x = matrix()) {

    get <- function() x
    
    set <- function(y) {
        # ** basic argument validity check **
        if (nrow(y) != ncol(y))
            warning("x is not a square matrix!")
        if (sum(colSums(is.na(y))))
            warning("x obviously non-invertible: it contains NAs.")
        
        x <<- y
        x.inv <<- NULL
    }
    
    p.getInv <- function() x.inv

    p.setInv <- function(inv) x.inv <<- inv
    
    
    # x is readily set but we explicitly call set(x) to take adv. of the check
    # on x value as well as to get x.inv set to NULL
    set(x)  
    
    # x.inv <<- NULL  # @@@@ see if <<- needed or even right...    

    # return "CacheMatrix" object
    list(get=get, set=set, p.getInv=p.getInv, p.setInv=p.setInv)
}

## cacheSolve()
##   Inverses a "CacheMatrix" object
##   Arguments x : a CacheMatrix (see makeCacheMatrix())
##          ...  : other arguments, passed as is to/from other methods
##   Return value: a matrix which is the inverse of x
cacheSolve <- function(x, ...) {
    retVal <- x$p.getInv()
    
    if(is.null(retVal)) {   # not already cached?
        mx <- x$get()
        retVal  <- solve(mx, ...)
        x$p.setInv(retVal)  # save it for next time.
    }
    else {
        message("getting cached data")
    }
    
    retVal
}

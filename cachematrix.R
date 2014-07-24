## allows changing/ displaying both original matrix and inverse matrix

makeCacheMatrix <- function(m = matrix(), ...) {
        
        ## 'inverse' is initialised
        inverse <- NULL
        
        ## allows inputting a new matrix, 'm' takes over values from argument 'n'
        set <- function(n) {
                m <<- n
                inverse <<- NULL
        }
        
        ## shows the matrix as it is currently saved
        get <- function() m  
        
        ## shows 'inverse'
        getInverse <- function() inverse
        
        ## allows changing of 'inverse', usually after calling cacheSolve
        setInverse <- function(i) inverse <<- i
        
        ## arranges all four functions inside makeCacheMatrix in a list, which allows
        ## to call each one separately by entering 'makeCacheMatrix(...)$...'
        list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)        
        
        
        
}


## this function checks whether the inverse matrix has already been calculated,
## and if not, calculates it

cacheSolve <- function(m, ...) {
        
        ## retrieve 'inverse' from makeCacheMatrix
        inverse <- m$getInverse()
        
        ## if the inverse matrix has already been calculated, cacheSolve stops at this
        ## point and returns that inverse matrix
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ## otherwise: the matrix is retrieved
        data <- m$get()
        
        ## calculation of the inverse matrix using solve()
        inverse <- solve(data)
        
        ## calling setInverse to save and return the inverse matrix
        m$setInverse(inverse)
        inverse
}

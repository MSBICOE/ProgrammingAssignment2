############################################################
##Steven Wang's Lexical Scoping Assignment
##Date: 19th Jun 2014
############################################################

# cache the matrix if there was an inverse matrix there for the input matrix

makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL		# do not assign anything to m (using cached value if any)
        set <- function (y)
        {
                x <<- y
                m <<- NULL		# Invalidate the cached matrix				
        }
        get <- function() { x }
        setInverse <- function (func)	m <<- func
        getInverse <- function() m
        list	(	set = set
               , get = get
               , setInverse = setInverse
               , getInverse = getInverse
        )				
}

## if there is a cached inverse matrix available then simply get the inverse matrix from cache
## if it is calculating inverse matrix for a new matrix, then perform the new calculation.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if (!is.null(m))
        {
                message("Getting the inverse matrix from cache")
                return(m)			
        }
        newMatrix <- x$get()
        m <- solve(newMatrix,...)
        x$setInverse(m)
        m
}

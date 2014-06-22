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
        setInverse <- function (func)	
        {
                m <<- func		#Recalculating the based on the func variable
        }
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
        #x <- makeCacheMatrix(x)
        m <- x$getInverse()
        if (!is.null(m))
        {
                message("Getting the inverse matrix from cache")
                return(m)			
        }
        newMatrix <- x$get()
        # check to see if the matrix is invertible
        myRow <- nrow(newMatrix)
        myCol <- ncol(newMatrix)
        # First to check if a matrix is a square one
        if (myRow != myCol)
        {
                message("The matrix is not invertible")
                m <- NA
                return(m)
        }
        # Second to check if the determinant of the matrix is 0
        comboMatrix <- cbind(newMatrix, newMatrix[,1:(myCol - 1)])	# combine the matrix to the left except the last column
        myint <- rep(1,length.out = (2 * myRow))	# initiate a vector value with 1 twice size as matrix rows
        myD <-data.frame(myint)
        for (i in 1:myRow)
        {
                for (j in 1:myCol)
                {
                        myD[j,] <- comboMatrix[i,i + j - 1] * myD[j,]	# calculate downwards diagonal value 
                        myD[j + myRow,] <- comboMatrix[myRow - i + 1, i + j - 1] * myD[j + myRow,]	# calculate upwards diagonal value 
                }
        }
        mydet <- sum(head(myD,myRow)) - sum(tail(myD,myRow))   # calculate the determinant
        if (mydet == 0)
        {
                message("The determinant of the matrix is 0 so the matrix is not invertible")
                m <- NA
                return(m)
        }
        # if not cached and it is invertible then do the inverse calculation
        m <- solve(newMatrix,...)
        x$setInverse(m)
        m
}

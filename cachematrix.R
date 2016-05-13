## This function caches the matrix inversion computation

## The function ``makeCacheMatrix`` creates a specia matrix. It os a list contains functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inversion
## 4. get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {        				#takes an argument x of matrix type
        s <- NULL        							#intitializaion of matrix inversion variable s
        set <- function(y) {        						#takes the argument y and assigns it as a matrix value
                x <<- y             						#stored as a variable x in the parent function environment
                s <<- NULL
        }
        get <- function() x        						#returns a pre-assigned matrix value to variable x
        setinv <- function(inverse) s <<- inverse        			#takes an argument (inverted matrix) and stores it as a variable s
        getinv <- function() s        						#returns the inverted matrix stored as variable s
        list( set = set, get = get, setinv = setinv, getinv = getinv)        	#the output is a list  of four functions 
}


##The function ``cacheSolve`` checks if inversion of matrix (x) has been calculated and cached. 
##if calculated and cached, it returns the cached data. 
##If not, it calculates the inversion and caches it for future calls.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinv()
        if (!is.null(s)) {        	#checks if the inversion has been calculated
                message("Getting cached data!")
                return (s)        	#if calculated, the value will be returned; no calculation needed
        }
        message("Calculating matrix inversion!")
        matrixx <- x$get()       	#if not calculated already, it gets the matrix
        s <- solve (matrixx, ...)       #, invert it, store it in a variable s
        x$setinv(s)        		#, and store it as an input for the setinv function (caching step)
        s       			#returns the inverted matrix
}

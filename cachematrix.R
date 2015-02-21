## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#---------------------------------------------------------------------------
# 1. function input: takes matrix as input e.g. matrix(c(1,2,3,2),nrow=2,ncol=2)
# 2. calls function set_mat which creates a cached matrix of above input
#    return a special type of object
# 3. Following function can be called on the object created by makeCacheMatrix
#    function get_mat: returns a cached matrix - used in future calls
#    function setmat_inv: caches inverse matrix - used in future calls
#    function getmat_inv: returns cached inverse matrix - used in future calls
# e.g. yy <- makeCacheMatrix(matrix(c(1,2,3,2),nrow=2,ncol=2))
#---------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL				#initialize
		set_mat <- function(y){	#function to assign values
				x <<-y			#save data matrix 
				m <<- NULL		#reset m
		}		
		get_mat <- function() x	#return data matrix
        setmat_inv <- function(mat_inv) m <<- mat_inv
        getmat_inv <- function() m		#return inverse matrix
        list(set_mat = set_mat, get_mat = get_mat,
             setmat_inv = setmat_inv,
             getmat_inv = getmat_inv)
}


## Write a short comment describing this function
#---------------------------------------------------------------------------
# 1. function input : takes a object created by makeCacheMatrix function 
# 2. Gets stored value of inverse matrix for input object using getmat_inv function
# 3. if stored value not null then we return this value and exit function call
# 4. if stored value not found we fetch data from input object cache, 
#    calculate its inverse and also store this value in cache for further use
#    e.g. yy <- makeCacheMatrix(matrix(c(1,2,3,2),nrow=2,ncol=2))
#		  cacheSolve(yy)
#---------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		mat_inv <- x$getmat_inv()		
		if(!is.null(mat_inv)) {		
				message("getting cached data")
				return(mat_inv)
		}
		mat_data <- x$get_mat()		#get stored data from matrixCacheMatix object
		mat_inv <- solve(mat_data)	#calculate inverse of matrix
		x$setmat_inv(mat_inv)		#save calculations for future use
		mat_inv						
}
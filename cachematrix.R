# Calculate an inverse matrix is usually a costly computation process.
# The functions "makeCacheMatrix" and "cacheSolve" are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix: "set_MatInv"
# 4. get the value of inverse of the matrix: "get_MatInv"
makeCacheMatrix <- function(MyMatrix = matrix()) 
{
        inv_matrix <- NULL ## inverse matrix
        set <- function(y) {
                MyMatrix <<- y
                inv_matrix <<- NULL
        }
  get<-function() MyMatrix
  set_MatInv <-function(MatInverse) inv_matrix <<- MatInverse
  get_MatInv<-function() inv_matrix
  list(set=set,get=get,set_MatInv=set_MatInv,get_MatInv=get_MatInv)
}


# The function "cacheSolve" returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# "set_MatInv" function.

# This function assumes that the matrix is always invertible.

cacheSolve<- function(MyMatrix) {
    inv_matrix <- MyMatrix$get_MatInv()
if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
}
data <- MyMatrix$get()
inv_matrix <- solve(data) ## calculus of matrix inverse
	MyMatrix$set_MatInv(inv_matrix)
inv_matrix
}

## Sample to test both functions:
## > x = rbind(c(2,0,-2),c(4,-2,4),c(-7,-2,6))
##> m<-makeCacheMatrix(x)
##> m$get()
##     [,1] [,2] [,3]
##[1,]    2    0   -2
##[2,]    4   -2    4
##[3,]   -7   -2    6

## No cache in the first run
## > cacheSolve(m)
##           [,1]        [,2]       [,3]
##[1,] -0.1111111  0.11111111 -0.1111111
##[2,] -1.4444444 -0.05555556 -0.4444444
##[3,] -0.6111111  0.11111111 -0.1111111

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]        [,2]       [,3]
##[1,] -0.1111111  0.11111111 -0.1111111
##[2,] -1.4444444 -0.05555556 -0.4444444
##[3,] -0.6111111  0.11111111 -0.1111111

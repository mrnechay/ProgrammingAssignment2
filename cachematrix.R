## These functions work togther to alleviate some compute time in finding
# the inverse of a matrix which can sometimes be a lengthy calculation.
# It does this by allowing the user to automatically cache and retrieve
# results if available.

# To start, a variable must be defined as the makeCacheMatrix function.
# This function first sets the space for a cached matrix to null and
# defines functions to set and get matrix which can be called from elsewhere

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setMatrix<-function(solve) m<<- solve
    getMatrix<-function() m
    list(set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)
}

# cacheSolve will first attempt to get the solved matrix from "cache", i.e.,
# from the environment of the variable defined by makeCacheMatrix. 
# If a value is present, the function alerts the user that cached data
# is being retrieved and the function ends with returning the already solved
# matrix. If not in cache, function continues to solve and store the solved matrix

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getMatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setMatrix(m)
    m
}
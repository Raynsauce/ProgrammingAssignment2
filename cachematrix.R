##Simply edited from the example given on Github for vectors;
makeCacheMatrix <- function(x = matrix()) {
        mtx <- NULL    #Init m, our matrix
        set <- function(y)
        {
            x <<- y     #Generic variable names ftw
            mtx <<- NULL
        }
        #Return the value of the current matrix
        get <- function(){ return(x) }
        setinvert <- function(inverse) mtx <<- inverse
        getinvert <- function(){ return(mtx) }

        return(list(set = set,get = get,
                    setinvert = setinvert,
                    getinvert = getinvert))
        #I prefer explicit return() statements
}

#Inverts the matrix after using makeCacheMatrix(),
#the result is stored in cache after computation,
#if it exists in cache, it's simply returned;
#This function assumed the matrix is invertable:
cacheSolve <- function(x, ...) {
    #Attempt to read in the cached matrix:
    mtx <- x$getinvert()
    if(!is.null(mtx)){  ##Check if the matrix exists
        message("Getting cached data...")
        return(mtx)
    }

    #No cached matrix exists; compute one:
    mtxdat <- x$get() #make a temporary to pass to solve()
    mtx <- qr.solve(mtxdat)
    #qr.solve is simply a faster version of solve()
    x$setinvert(mtx)  #This pushes m back into the cache

    return(mtx)
} #Newline at the end to make Github happy

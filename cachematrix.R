## this function will ONLY cache square matrices ie. 3x3
makeCacheMatrix <- function(x) {
  m <- NULL ##placeholder for cached solved matrix
  q<-dim(x) ##used to test matrix
  bad<-"BZZZ! No dice, your matrix doesn't make sense"
  if(q[1]!=sqrt(length(x))){
    return(bad)
    ##determines if matrix is square,
    ##returns error if the matrix is NOT square
  }
  if(det(x)==0){
    return(bad)
    ##determines if inverse is possible, 
    ##returns error if the matrix is singular
  }
  get <- function() x             ##caches value of original matrix 'x'
  slvm <- function(slv) m <<- slv ##caches inverse of matix 'x'
  getm <- function() m            ##caches computation of inverse
  list(get = get,
       slvm = slvm,
       getm = getm)
  ##list order of functions in the global environment
}

##this function returns the inverse matrix of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getm()     ##pulls past computation of inverse and sets to 'm'
  if(!is.null(m)) { ##test if 'm' is NOT empty
    message("getting cached data")
    return(m)       ##prints 'm' if NOT empty
  }
  data <- x$get()   ##pulls original matrix and sets to 'data'
  m <- solve(data)  ##solves for matrix inverse and sets to 'm'
  x$slvm(m)         ##sets solved inverse to 'slvm'
  m                 ##prints 'm'
}
## 14Apr2017 - Javier Navarro B
## Programming 2:  Given a matrix its Inverse is returned by these 2 functions using Lexical Scoping

## This function returns the Inverse of a matrix given as parameter in the variable m_JNB

makeCacheMatrix <- function(m_JNB = matrix()){
      inv_JNB   <- NULL
      setJNB    <- function(y_JNB){
            m_JNB   <<- y_JNB
            inv_JNB <<- NULL
      }
      getJNB    <- function() m_JNB
      setInvJNB <- function(matinverse) inv_JNB <<- matinverse
      getInvJNB <- function() inv_JNB
      list(setJNB=setJNB,
           getJNB=getJNB,
           setInvJNB=setInvJNB,
           getInvJNB=getInvJNB
      )
}


## This function returns the Inverse of a Matrix

cacheSolve <- function(m_JNB, ...) {
      inv_JNB    <- m_JNB$getInvJNB()
      if(!is.null(inv_JNB)) {
            message("getting cached data")
            return(inv_JNB)
      }
      matrix_JNB <- m_JNB$getJNB()
      inv_JNB    <- solve(matrix_JNB)
      m_JNB$setInvJNB(inv_JNB)
      inv_JNB
}

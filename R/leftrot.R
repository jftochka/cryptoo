#' Rotates the elements of a vector to the left
#'
#' @param v Vector to be rotated
#' @param rot Number of positions to rotate
#'
#' @return Rotated vector
#' @export
#'
#' @examples
#' leftrot(1:10,2)
#' leftrot(letters,3)
leftrot <- function(v,rot){
  l <- length(v)
  v[(0:(l-1)+rot)%%l+1]
}

uintToBits <- function(int,size=32,little=FALSE){
  if(int>=2^size){
    warning("overflow")
  }
  bits <- numeric(size)
  index <- 1:size
  if(!little){
    index <- size:1
  }
  for(i in index){
    bit <- int%%2
    int <- (int-bit)/2
    bits[i] <- bit
  }
  bits
}

bitsToUint <- function(bits,size=32,little=FALSE){
  if(length(bits)!=size){
    warning("size doesnt match")
  }
  powers <- 2^(1:size -1)
  if(!little){
    powers <- 2^(size:1 -1)
  }
  sum(bits*powers)
}

leftrot32 <- function(x,shift){
  x <- uintToBits(x,32,FALSE)
  x <- leftrot(x,shift)
  bitsToUint(x,32,FALSE)
}

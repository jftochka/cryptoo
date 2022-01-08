
#' Secure Hash Algorithm 1
#'
#' This implementation is bugged.
#' Already deprecated hash function SHA1. Replaced by SHA3.
#'
#' @param msg Message to hash. Given as a bytestream (numeric array with values from 0 to 255).
#'
#' @return Hashed value as a string.
#' @export
#'
#' @examples
#' sha1(utf8ToInt(""))
sha1 <- function(msg){
  #init
  a0 <- 0x67452301
  b0 <- 0xEFCDAB89
  c0 <- 0x98BADCFE
  d0 <- 0x10325476
  e0 <- 0xC3D2E1F0
  extend <- function(x1,x2,x3,x4){
    x1 <- uintToBits(x1,32,FALSE)
    x2 <- uintToBits(x2,32,FALSE)
    x3 <- uintToBits(x3,32,FALSE)
    x4 <- uintToBits(x4,32,FALSE)
    bitsToUint(xor(xor(x1,x2),xor(x3,x4)))
  }
  F1 <- function(x,y,z){
    x <- uintToBits(x,32,FALSE)
    y <- uintToBits(y,32,FALSE)
    z <- uintToBits(z,32,FALSE)
    bitsToUint((x&y)|(!x&z) )
  }
  F2 <- function(x,y,z){
    x <- uintToBits(x,32,FALSE)
    y <- uintToBits(y,32,FALSE)
    z <- uintToBits(z,32,FALSE)
    bitsToUint(xor(xor(x,y),z) )
  }
  F3 <- function(x,y,z){
    x <- uintToBits(x,32,FALSE)
    y <- uintToBits(y,32,FALSE)
    z <- uintToBits(z,32,FALSE)
    bitsToUint( (x&y)|(x&z)|(y&z) )
  }
  F <- c(F1,F2,F3,F2)
  K <- c(floor(2^30*sqrt(c(2,3,5,10))))
  #preprocesing
  len <- (length(msg)*8)%%2^64
  lenbits <- uintToBits(len,64,little=TRUE)
  lenbytes <- numeric(8)
  for(i in 0:7){
    lenbytes[i+1] <- bitsToUint(lenbits[8*i+1:8],8,little=TRUE)
  }
  msg <- c(msg,0x80)
  while(length(msg)%%64!=56){
    msg <- c(msg,0)
  }
  msg <- c(msg,lenbytes)
  #procesing
  for(j in 1:(length(msg)/64)){
    chunk <- msg[64*(j-1)+1:64]
    M <- numeric()
    for(i in 1:16){
      M[i] <- sum(chunk[4*(i-1)+1:4]*2^(8*0:3))
    }
    for(i in 16:79){
      M[i+1] = leftrotate(extend(M[i-3+1],M[i-8+1],M[i-14+1],M[i-16+1]),1)
    }
    a <- a0
    b <- b0
    c <- c0
    d <- d0
    e <- e0
    for(i in 0:79){
      ind <- i%/%20
      f <- F[[ind+1]](b,c,d)
      k <- K[ind+1]
      temp <- (leftrotate(a,5)+f+e+k+M[i+1])%%2^32
      e <- d
      d <- c
      c <- leftrotate(b,30)
      b <- a
      a <- temp
    }
    a0 <- (a0+a)%%2^32
    b0 <- (b0+b)%%2^32
    c0 <- (c0+c)%%2^32
    d0 <- (d0+d)%%2^32
    e0 <- (e0+e)%%2^32
  }
  digest <- c(a0,b0,c0,d0,e0)
  #postprocessing
  digestbit <- c(uintToBits(a0,32,little=FALSE),uintToBits(b0,32,little=FALSE),uintToBits(c0,32,little=FALSE),uintToBits(d0,32,little=FALSE),uintToBits(e0,32,little=FALSE))
  digestbytes <- numeric(16)
  for(i in 0:19){
    digestbytes[i+1] <- bitsToUint(digestbit[i*8+1:8],8,FALSE)
  }
  digesthex <- paste(as.raw(digestbytes),collapse="")
  digesthex
}


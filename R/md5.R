
#' Message Digest 5
#'
#' Already deprecated hash function md5. Replaced by SHA3.
#'
#' @param msg Message to digest. Given as a bytestream (numeric array with values from 0 to 255).
#'
#' @return Hashed value as a string.
#' @export
#'
#' @examples
#' md5(utf8ToInt(""))
md5 <- function(msg){
  #init
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
    bitsToUint((x&z)|(y&!z) )
  }
  F3 <- function(x,y,z){
    x <- uintToBits(x,32,FALSE)
    y <- uintToBits(y,32,FALSE)
    z <- uintToBits(z,32,FALSE)
    bitsToUint(xor(xor(x,y),z) )
  }
  F4 <- function(x,y,z){
    x <- uintToBits(x,32,FALSE)
    y <- uintToBits(y,32,FALSE)
    z <- uintToBits(z,32,FALSE)
    bitsToUint(xor(x|!z,y) )
  }
  F <- c(F1,F2,F3,F4)
  G <- matrix(c(1,5,3,7,0,1,5,0),ncol=2)
  S <- c(rep(c(7,12,17,22),4),rep(c(5,9,14,20),4),rep(c(4,11,16,23),4),rep(c(6,10,15,21),4))
  K <- numeric(64)
  for(i in 0:63){
    K[i+1] <- floor(2^32*abs(sin(i+1)))
  }
  a0 <- 0x67452301
  b0 <- 0xefcdab89
  c0 <- 0x98badcfe
  d0 <- 0x10325476
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
    A <- a0
    B <- b0
    C <- c0
    D <- d0
    for(i in 0:63){
      fun <- i%/%16+1
      g <- (i*G[fun,1]+G[fun,2])%%16
      f <- F[[fun]](B,C,D)
      f <- (f+A+K[i+1]+M[g+1])%%2^32
      A <- D
      D <- C
      C <- B
      B <- (B+leftrot32(f,S[i+1]))%%2^32
    }
    a0 <- (a0+A)%%2^32
    b0 <- (b0+B)%%2^32
    c0 <- (c0+C)%%2^32
    d0 <- (d0+D)%%2^32
  }
  digest <- c(a0,b0,c0,d0)
  #postprocessing
  digestbit <- c(uintToBits(a0,32,little=TRUE),uintToBits(b0,32,little=TRUE),uintToBits(c0,32,little=TRUE),uintToBits(d0,32,little=TRUE))
  digestbytes <- numeric(16)
  for(i in 0:15){
    digestbytes[i+1] <- bitsToUint(digestbit[i*8+1:8],8,TRUE)
  }
  digesthex <- paste(as.raw(digestbytes),collapse="")
  digesthex
}

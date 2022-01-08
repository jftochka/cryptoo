
#' rc4
#'
#' Already deprecated stream cipher rc4.
#' @param msg Message to encrypt. Given as a bytestream (numeric array with values from 0 to 255).
#' @param key Key used for encryption. Given as a bytestream.
#'
#' @return Cipher text as a string.
#' @export
#'
#' @examples
rc4 <- function(msg,key){
  #key scheduling algorithm
  ksa <- function(key){
    lenkey <- length(key)
    S <- 0:255
    j <- 0
    for(i in 0:255){
      j <- (j+S[i+1]+key[(i%%lenkey)+1])%%256
      S[c(i+1,j+1)] <- S[c(j+1,i+1)]
    }
    S
  }
  #pseudo random generation algorithm
  prga <- function(S,lenmsg){
    i <- 0
    j <- 0
    t <- numeric(lenmsg)
    for(l in 1:lenmsg){
      i <- (i+1)%%256
      j <- (j+S[i+1])%%256
      S[c(i+1,j+1)] <- S[c(j+1,i+1)]
      t[l] <- (S[i+1]+S[j+1])%%256
    }
    S[t+1]
  }
  S <- ksa(key)
  keystream <- prga(S,length(msg))
  ciphertxt <- bitwXor(msg,keystream)
  paste(as.raw(ciphertxt),collapse="")
}

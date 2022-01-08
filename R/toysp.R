#' Toy Substitution-Permutation block cipher
#'
#' Block cipher with substitution permutation and a xor key.
#'
#' @param block Block as bit stream.
#' @param key Key as bit stream.
#' @param rounds Rounds.
#' @param s Substitution box n·n, as an array with numbers between 0 and 2^n-1.
#' @param p Permuation, as an array with numbers between 1 and block size.
#'
#' @return Encrypted block as bit stream.
#' @export
#'
#' @examples
#' s <- c(2,3,1,13,4,5,6,7,15,10,8,9,0,12,14,11)
#' p <- seq(0,5*127,5)%%128+1
#' key <- sample(c(0,1),128,TRUE)
#' rounds <- 4
#' block1 <- sample(c(0,1),128,TRUE)
#' toysp(block1,key,rounds,s,p)
toysp <- function(block,key,rounds,s,p){
  lenblock <- length(block)
  ssize <- log(length(s),2)
  samount <- lenblock/ssize
  for(round in 1:rounds){
    for(i in 1:samount){
      x <- block[(ssize*i-ssize+1):(ssize*i)]
      x <- bitsToUint(x,size=ssize)
      x <- s[x+1]
      x <- uintToBits(x,size=ssize)
      block[(ssize*i-ssize+1):(ssize*i)] <- x
    }
    block <- block[p]
    block <- bitwXor(block,key)
  }
  block
}

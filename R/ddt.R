#' Diferential Distribution Table
#'
#' Computes the diferential distribution table of a n·n Sbox
#'
#' @param sbox Sbox.
#'
#' @return ddt of the Sbox.
#' @export
#'
#' @examples
#' s <- c(2,3,1,13,4,5,6,7,15,10,8,9,0,12,14,11)
#' ddt(s)
ddt <- function(sbox){
  lens <- length(sbox)
  ddt <- matrix(0,nrow=lens,ncol=lens)
  for(i in 0:(lens-1)){
    difs <- numeric(lens)
    for(j in 0:(lens-1)){
      difs[j+1] <- bitwXor(sbox[j+1],sbox[bitwXor(j,i)+1])
    }
    ddt[i+1,as.integer(names(table(difs)))+1] <- table(difs)
  }
  ddt
}

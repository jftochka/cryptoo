#' A5/1
#'
#' Already deprecated stream cipher A5/1.
#'
#' @param seed Bit stream used as seed.
#' @param nonce Bit stream used as nonce.
#'
#' @return Key stream, to be Xored with plaintext to produce the ciphertext
#' @export
#'
#' @examples
a5 <- function(seed,nonce){
  stopifnot(length(seed)==64)
  stopifnot(length(nonce)==22)
  seed <- c(seed,nonce)
  register <- list(numeric(19),numeric(22),numeric(23))
  pol <- list(c(19,18,17,14),c(22,21),c(23,22,21,8))
  clockbit <- c(9,11,11)
  for(i in 1:86){
    for(j in 1:3){
      register[[j]][1] <- as.numeric(xor(register[[j]][1],seed[i]))
      register[[j]] <- clock(register[[j]],pol[[j]])
    }
  }
  stream <- numeric(328)
  for(i in 1:328){
    maj <- majority(register[[1]][clockbit[1]],register[[2]][clockbit[2]],register[[3]][clockbit[3]])
    stream[i] <- 0
    for(j in 1:3){
      stream[i] <- xor(stream[i],register[[j]][length(register[[j]])])
      if(register[[j]][clockbit[j]]==maj){
        register[[j]] <- clock(register[[j]],pol[[j]])
      }
    }
  }
  stream[101:328]
}

clock <- function(reg,pol){
  newbit <- sum(reg[pol])%%2
  reg <- leftrot(reg,-1)
  reg[1] <- newbit
  reg
}

majority <- function(x,y,z){
  sum(x*y,x*z,y*z)%%2
}



#' Caesar cipher
#'
#' Monoalphabetic(only 1 way to substitute letters) monogramic(letter by letter) cipher.
#'
#' @param msg Message.
#' @param key Number of shifts.
#' @param decrypt Encrypt or decrypt mode.
#' @param alf_size Alphabet size.
#'
#' @return Encrypted message.
#' @export
#'
#' @examples
#' crypto <- caesar(msg_en,key=3)
#' crypto
#' caesar(crypto,key=3,decrypt=TRUE)
caesar <- function(msg,key,decrypt=FALSE,alf_size=26){
  msg <- preprocess(msg)
  if(decrypt){
    key <- alf_size-key
  }
  msg <- (msg+key)%%alf_size
  msg <- postprocess(msg)
  msg
}

#' Vigenere cipher
#'
#' Polyalphabetic(many ways to substitute letters) monogramic(letter by letter) cipher.
#'
#' @param msg Message.
#' @param key Password.
#' @param decrypt Encrypt or decrypt mode.
#' @param alf_size Alphabet size.
#'
#' @return Encrypted message.
#' @export
#'
#' @examples
#' crypto <- vigenere(msg_en,"abc")
#' crypto
#' vigenere(crypto,"abc",decrypt=TRUE)
vigenere <- function(msg,key,decrypt=FALSE,alf_size=26){
  msg <- preprocess(msg)
  key <- preprocess(key)
  lenmsg <- length(msg)
  lenkey <- length(key)
  key <- key[((1:lenmsg-1)%%lenkey)+1]
  if(decrypt){
    key <- alf_size-key
  }
  msg <- (key-msg)%%alf_size
  msg <- postprocess(msg)
  msg
}

#' Beaufort cipher
#'
#' Polyalphabetic(many ways to substitute letters) monogramic(letter by letter) cipher.
#' Similar to Vigenere, but encrypt and decrypt are the same function.
#'
#' @param msg Message.
#' @param key Password.
#' @param alf_size Alphabet size.
#'
#' @return Encrypted message.
#' @export
#'
#' @examples
#' crypto <- beaufort(msg_en,"abc")
#' crypto
#' beaufort(crypto,"abc")
beaufort <- function(msg,key,alf_size=26){
  msg <- preprocess(msg)
  key <- preprocess(key)
  lenmsg <- length(msg)
  lenkey <- length(key)
  key <- key[((1:lenmsg-1)%%lenkey)+1]
  msg <- (key-msg)%%alf_size
  msg <- postprocess(msg)
  msg
}

#' Vigenere autokey cipher
#'
#' Polyalphabetic, monogramic, aperiodic cipher. Plain text becomes part of the key.
#'
#' @param msg Message.
#' @param key Password.
#' @param decrypt Encrypt or decrypt mode.
#' @param alf_size Alphabet size.
#'
#' @return Encrypted message.
#' @export
#'
#' @examples
#' crypto <- autokey(msg_en,"abc")
#' crypto
#' autokey(crypto,"abc",decrypt=TRUE)
autokey <- function(msg,key,decrypt=FALSE,alf_size=26){
  msg <- preprocess(msg)
  key <- preprocess(key)
  lenmsg <- length(msg)
  lenkey <- length(key)
  key <- c(key,msg)[1:lenmsg]
  if(decrypt){
    key <- alf_size-key
    if(lenmsg>lenkey){
      for(i in 1:(lenmsg-lenkey)){
        key[i+lenkey] <- key[i+lenkey]-key[i]
      }
    }
  }
  msg <- (msg+key)%%alf_size
  msg <- postprocess(msg)
  msg
}

#' Frequency analysis
#'
#' Caesar cipher can be decrypted without the key if the language of the message is known.
#'
#' @param msg Ciphertext.
#' @param freq Letter's frequency in the message's language.
#'
#' @return Caesar cipher secret key. Sorted according to their likelihood.
#' @export
#'
#' @examples
#' crypto <- cesar(msg_en,3)
#' crypto
#' info <- freq_analysis(preprocess(crypto),freqs$en)
#' info
#' cesar(crypto,info[1],decrypt=TRUE)
freq_analysis <- function(msg,freq){
  freq <- unname(freq)
  alf_size <- length(freq)
  obsf <- table(factor(msg,levels=1:alf_size-1))/length(msg)
  dist <- numeric(alf_size)
  for(key in 1:alf_size){
    spectf <- freq[leftrot(1:alf_size,-key)]
    dist[key] <- sum((spectf-obsf)^2)
  }
  order(dist)%%alf_size
}

#' Kasiski attack
#'
#' Vigenere cipher can be decrypted without the key. First find the key length.
#' Then divide ciphertext into as many sub-ciphertexts as the key length.
#'
#' @param msg Ciphertext.
#' @param freq Letter's frequency in the message's language.
#' @param lenkey Key length can be provided to avoid compute it.
#'
#' @return
#' @export
#'
#' @examples
#' crypto <- vigenere(msg_en,"abc")
#' crypto
#' info <- kasiski(crypto,freqs$en)
#' info
#' vigenere(crypto,info$key,decrypt=TRUE)
kasiski <- function(msg,freq,lenkey=0){
  #preprocessing
  alf_size <- length(freq)
  if(alf_size>64 && lenkey==0){
    stop("alphabet size > 64, key-length will not be obtained correctly")
  }
  msg <- preprocess(msg)
  lenmsg <- length(msg)
  #get lenkey
  if(lenkey==0){
    tetragram <- list()
    for(i in 1:(lenmsg-3)){
      tetragram[[i]] <- msg[i:(i+3)]
    }
    for(i in 1:length(tetragram)){
      tetragram[[i]] <- sum(c(2^18,2^12,2^6,2^0)*tetragram[[i]])
    }
    tetragram <- as.numeric(tetragram)
    repeated <- table(tetragram)
    repeated <- repeated[repeated>1]
    positions <- list()
    for(i in 1:length(repeated)){
      positions[[i]] <- which(tetragram==names(repeated[i]))
    }
    differences <- list()
    gcds <- numeric()
    for(i in 1:length(positions)){
      differences[[i]] <- diff(positions[[i]])
      gcds[i] <- gcd(differences[[i]])
    }
    lenkey <- cgcd(gcds)
  }else{
    gcds <- lenkey
  }
  #split into lenkey submsgs
  submsg <- list()
  for(i in 1:lenkey){
    ind <- (((1:lenmsg-1)%%lenkey)+1)==i
    submsg[[i]] <- msg[ind]
  }
  keys <- matrix(NA,nrow=lenkey,ncol=alf_size)
  for(i in 1:lenkey){
    keys[i,] <- freq_analysis(submsg[[i]],freq)
  }
  keys <- postprocess(keys,multiple=TRUE)
  dim(keys) <- c(lenkey,alf_size)
  key <- paste(keys[,1],collapse="")
  list("key"=key,"keys"=keys,"gcds"=gcds)
}

#greatest common divisor
gcd <- function(v){
  for(i in 1:min(v)){
    if(all(v%%i==0)){
      gcd <- i
    }
  }
  gcd
}

#commonest greatest common divisor
cgcd <- function(gcds){
  table(gcds)
  sort(table(gcds),decreasing=TRUE)
  common <- as.numeric(names(sort(table(gcds),decreasing=TRUE)[1]))
  if(mean((gcds%%common)==0)>0.9 || mean(gcds==common)>0.35){
    return(common)
  }
  return(gcd(gcds))
}

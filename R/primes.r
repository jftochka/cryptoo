

sieve <- function(x){
  primes <- 2
  for(i in 2:x){
    s <- ceiling(sqrt(i))
    pflag <- TRUE
    for(p in primes[primes<=s]){
      pflag <- pflag & i %% p
    }
    if(pflag){
      primes <- c(primes,i)
    }
  }
  primes
}

is_prime <- function(x){
  if(x==2){
    return(TRUE)
  }
  s <- ceiling(sqrt(x))
  primes <- sieve(s)
  for(p in primes){
    if(x %% p ==0){
      return(FALSE)
    }
  }
  return(TRUE)
}

factorize <- function(x){
  s <- ceiling(sqrt(x))
  primes <- sieve(s)
  factorization <- c()
  while(x>1 & !is_prime(x)){
    for(p in primes){
      if(x%%p==0){
        factorization[length(factorization)+1] <- p
        x <- x/p
      }
    }
  }
  if(x==1){
    return(factorization)
  }
  c(factorization,x)
}





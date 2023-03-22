

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
#' crypto <- caesar(msg_ua,key=3)
#' crypto
#' caesar(crypto,key=3,decrypt=TRUE)
caesar <- function(msg,key,decrypt=FALSE,alf_size=33){
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
#' crypto <- vigenere(msg_ua,"abc")
#' crypto
#' vigenere(crypto,"abc",decrypt=TRUE)
vigenere <- function(msg,key,decrypt=FALSE,alf_size=34){
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
#' crypto <- beaufort(msg_ua,"abc")
#' crypto
#' beaufort(crypto,"abc")
beaufort <- function(msg,key,alf_size=34){
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
#' crypto <- autokey(msg_ua,"abc")
#' crypto
#' autokey(crypto,"abc",decrypt=TRUE)
autokey <- function(msg,key,decrypt=FALSE,alf_size=34){
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
#' crypto <- caesar(msg_ua,3)
#' crypto
#' info <- freq_analysis(preprocess(crypto),freqs$ua)
#' info
#' caesar(crypto,info[1],decrypt=TRUE)
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
#' crypto <- vigenere(msg_ua,"abc")
#' crypto
#' info <- kasiski(crypto,freqs$ua)
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

preprocess <- function(msg){
  ini <- c("áéíóúüñ")
  fin <- c("aeiouun")
  ini <- iconv(ini,"win1251","UTF-8")
  fin <- iconv(fin,"win1251","UTF-8")
  ini <- utf8ToInt(ini)
  fin <- utf8ToInt(fin)
  msg <- iconv(msg,"win1251","UTF-8")
  msg <- utf8ToInt(msg)
  for(i in 1:length(ini)){
    msg[msg==ini[i]] <- fin[i]
  }
  valid <- c("АБВГҐДЕЄЖЗИIЇЙКЛМНОПРСТУФХЦЧШЩЬЮЯ_")
  valid <- iconv(valid,"win1251","UTF-8")
  valid <- utf8ToInt(valid)
  msg <- msg[msg %in% valid]
  for(i in 1:length(valid)){
    msg[msg==valid[i]] <- i-1
  }
  msg
}

postprocess <- function(msg,multiple=FALSE){
  intToUtf8(msg+97,multiple)
}

msg_ua <- "У_НСВ_ҐУПМНННОВХЮЄББВНФПКЯ_АХНПҐУЇБЯҐНҐНГ_ДЖ_ФЯБЙНГСҐЖАВУИЙРНФПЖҐЄГКУМННДЙНҐСГРҐЕСҐЇНЧЯЩОО_ІБЙНСБЄСШНЯОҐЩЕІНЖЖЖЦНФҐШЯКМЩЕКАРМНЩЄТОЕ_ҐМОННДЙНР_БЇНДІВДВУЯЩБГҐ_ЯНОУҐЕЯНЇЬМЧНЩФНМОГЦЕР_ЩЙНЄВН_ЯТЄСҐНЄРОЯОЄИМГКБИЩЩОҐВНСЖЦЩНШНЄВДЦНПВДХЕЯНУҐҐПЄРЛЕХНҐ_ЦЖРЄКГЖҐЄЩЄБІШКҐЇЧШЩБЖНРРМДБЄГШБГФБЄМЄНЦБЬВС_НПФГБШТВУЯВДВНЩВНКВБЄБРАХДПФНЯОБІФВЕЯНЕҐҐХНГСҐЖАВУИЙРНЯОБАФЯФБІФЩОФШАҐЕЯНЮКЩПЇЩКГЙУНЖОУНУ_РДВЬКГРУВБВИЩНАВФМС_АБЯОАЩФВ_ЦЮУКЯЦНУ_ЄУЯЇНУ_СРУЦЛЕЇАЄГКБЯВУБЙЮНЮВКЮНФОГОСЛБШЩНАФТФУКГХП_ЦІПШАХНҐШДГЄСОАХНОФБСНСҐШЖФСЄҐЇРВЕШЮФБГЩРСРЕВНІВНЯ_АВБУЛНГЄУСНЛНАЦЮБДЕХ_ЙДЕКЩБУҐҐСБИШЯБВНЙ_НІАВРФНЛРЛРБҐВФЯЙЇБСНҐПҐЛБПМНЙЯЖНЄАФТЇФКЩБЧРСҐПЄ__НПОНЮФПФНҐРВСЄКСНМУЛІЙРНОВНВУЯШЧНЩОВТНА_МБПННВНМНЙНГШНАВУЕОБОНОДЙНШЩ_ЩВОНЧЛПХЕШКСР_ВРБЙНЩОҐЇНЩКБЙЯВЬШБПНЯФБЧСЛУХРОЬОБЬЗКЄДВ__ДБРУЛ_ВНПКЙРНЕ_НОФБЦНПОДЯШҐДМКГРСОЯШЛНД_ББО_СНФЄҐКГЖҐФЮВИИЩННШФНҐБЖАВУ_ЇР_ЇХБРЩЯНПОРЮШУБФКЧВГ_ЛБРРОЯШЛНГ_АЙЦНИВТЩІЯШОШНЦСТОАХНФОЯХАБІШЯВОНЯЛГЙЕВЬЩЛНЧЮЄЬЄХКБВДНЯФБЧОМЄҐОЕХНИОЗКЧРРДЦАБГВБМЄНЙ_ҐРЯЄКБВАНЯФБ_ШЗФББОООЄЄЛГКБГҐ_НУЕОЬФҐЄНЕРВЕЯҐНҐ_ОРШБЯОАЩФВ_ЦМББОВНІВП_Р_МЧҐЛФКНЄФБДРЙЕМЄРЛЕЇНОКИЯВС_НТВЯҐНАНВРБВНЧКЧЙҐВЩНЬВЄКЩБАОЇНСВУ_ҐРЦЄНОФШНФНРЯҐҐСХНРКВМҐЄОНУЕВЙЕЇНГСҐЖУНЯОФВРАВОНЙ_НСВЧЦҐПВННЩФОХКОНФНГОЗАБ_НПФБЛРЇУШГКБАФЯФБУШНШФШДЙНҐНВЖЩБПОГКМОАКІЇЬЗКУЙЕФШНАНРМШНОНН_ВДБ_ҐХІ"
freqs <- list()
freqs$ua <- c(
  0.064, 0.013, 0.046, 0.013, 0.000, 0.027, 0.042, 0.005, 0.007, 0.020, 0.055, 0.044, 0.010,
  0.009, 0.033, 0.027, 0.029, 0.068, 0.086, 0.025, 0.043, 0.037, 0.045, 0.027, 0.003, 0.011, 0.010,
  0.011, 0.005, 0.004, 0.016, 0.008, 0.019, 0.138)
names(freqs$ua) <- c(
  "А", "Б", "В", "Г", "Ґ", "Д", "Е", "Є", "Ж", "З",
  "И", "I", "Ї", "Й", "К", "Л", "М", "Н", "О", "П", "Р",
  "С", "Т", "У", "Ф", "Х", "Ц", "Ч", "Ш", "Щ", "Ь", "Ю",
  "Я","_")
simplify_freq <- function(freq){
  ini <- c("áéíóúüñ")
  fin <- c("aeiouun")
  val <- c("abcdefghijklmnopqrstuvwxyz")
  ini <- iconv(ini,"win1251","UTF-8")
  fin <- iconv(fin,"win1251","UTF-8")
  val <- iconv(val,"win1251","UTF-8")
  ini <- utf8ToInt(ini)
  fin <- utf8ToInt(fin)
  val <- utf8ToInt(val)
  for(i in 1:length(ini)){
    freq[intToUtf8(fin[i])] <- freq[intToUtf8(fin[i])] + freq[intToUtf8(ini[i])]
  }
  freq <- freq[intToUtf8(val,TRUE)]
  freq
}
freqs$es34 <- simplify_freq(freqs$es)

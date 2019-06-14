#Phoneme code
#' Generate Phoneme Probabilities
#'
#' Genetrate a vector of the probability to know each phoneme.
#' @param P A list of parameters.
#' @keywords Phonemes
#' @export
#'
GeneratePhonemeProbabilities <- function(P){
  #get cosonant and vowel probs, order them
  if(P$PhoProbT == "Equal"){
    return(rep(.1,P$nPhon))
  }
  
  if(P$PhoProbT == "Frequency"){
    return(rep(1,P$nPhon))
  }
  
  if(P$PhoProbT == "Random"){
    PhonemeVector <- runif(P$nPhon)
    return(PhonemeVector[order(PhonemeVector, decreasing = TRUE)])
  }
  
  
  if(P$PhoProbT == "Real"){
    PhonemeVectorV <- GetRealPhonemeData(P$nVowl, TRUE, FALSE)
    PhonemeVectorC <- GetRealPhonemeData(P$nConse, TRUE, TRUE)
    
  }else if(P$PhoProbT == "RealMimic"){
    PhonemeVectorV <- GetRealPhonemeData(P$nVowl, FALSE, FALSE)
    PhonemeVectorC <- GetRealPhonemeData(P$nConse, FALSE, TRUE)
  }
  PhonemeVectorC <- PhonemeVectorC[order(PhonemeVectorC, decreasing = TRUE)]
  PhonemeVectorV <- PhonemeVectorV[order(PhonemeVectorV, decreasing = TRUE)]
  return(c(PhonemeVectorC,PhonemeVectorV))
}

#' Get Real Phoneme Data
#'
#' Uses the real Phoneme data from Creanza..... UPDATE THIS!!!! to determine the phoneme probabilities.
#' @param nPhoneme The number of phonemes (vowels or consonants).
#' @param actual Whether the data is Real (TRUE) or RealMimic (FALSE).
#' @param vowel If true load the vowel data, otherwise load the cosonant data.
#' @keywords Phonemes
#' @export
#'
GetRealPhonemeData <- function(nPhoneme, actual, vowel=FALSE){
  #load the data
  if(vowel){
    Data <- colSums(Vowels)
  }else{
    Data <- colSums(Consonants)
  }
  
  Data <- Data[-which(Data==0)]
  TotalPhonemes <- length(Data)
  Data <- Data/2082
  if(actual){
    return(Data)
  }
  
  #break into groups
  Legendary <- Data[which(Data > 0 & Data <= .025)]
  Legendary <- rpert(ceiling(nPhoneme*(length(Legendary)/TotalPhonemes)), min=0, max=.025, mode=mean(Legendary))
  Rare <- Data[which(Data > .025 & Data <= .1)]
  Rare <- rpert(ceiling(nPhoneme*(length(Rare)/TotalPhonemes)), min=.025, max=.1, mode=mean(Rare))
  Uncommon <- Data[which(Data > .1 & Data <= .3)]
  Uncommon <- rpert(ceiling(nPhoneme*(length(Uncommon)/TotalPhonemes)), min=.1, max=.3, mode=mean(Uncommon))
  Common <- Data[which(Data > .3 & Data <= 1)]
  Common <- rpert(nPhoneme-(length(c(Legendary,Rare,Uncommon))), min=.3, max=1, mode=mean(Common))
  return(c(Legendary,Rare,Uncommon,Common))
}

#' Generate Seed Language
#'
#'
#' @param P A list of parameters.
#' @param phonemeProbab The probability of gaining each phoneme in the population.
#' @param seedNum Which population seed is having it's language generated.
#' @keywords Phonemes
#' @export
#'
GenerateSeedLanguage <- function(P, phonemeProbab, seedNum){
  #how many vols and conso to draw
  if(!is.na(P$NumPopPho[seedNum])){
    Languagesize <- P$NumPopPho[seedNum]-(P$MinCon+P$MinVow)
  }else{
    Languagesize <- round(rpert(min=P$AvePho[1],mode=P$AvePho[2],max=P$AvePho[3],1))-(P$MinCon+P$MinVow) 
  }
  if(Languagesize > 0){
    Vowels <- ceiling(Languagesize*P$nVowl/P$nPhon)
    Consonants <- Languagesize-Vowels+P$MinCon
    Vowels <- Vowels+P$MinVow
  }else{
    Consonants <- P$MinCon
    Vowels <- P$MinVow
  }
  
  #draw vowls and conso
  Consonants <- sample(1:P$nConse,Consonants,prob=phonemeProbab[1:P$nConse])
  Vowels <- sample((P$nConse+1):P$nPhon,Vowels,prob=phonemeProbab[(P$nConse+1):P$nPhon])
  
  #create language vector
  Language <- matrix(0, ncol=P$nPhon)
  Language[,c(Consonants,Vowels)] <- 1
  return(Language)
}

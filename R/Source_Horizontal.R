#Allow for horizontal transfer

#' Horitontal Transfer Repeater
#'
#' A wrapper for the horizontal transfer process.  After migration, allow populations to exchnage phoneme information, losing or gaining syllables based on other populations in the simulation.  Occurs HSims number of time steps.
#' @param P A list of parameters.
#' @param S A list of the data structures.
#' @param repeats how many times to repeat horizontal transfer
#' @keywords Horizontal
#' @export
#'
HoritontalTransferRepeater <- function(P, S, repeats){
  PopulationPool <- which(S$OccupiedVacant)
  if(any(rowSums(S$Languages) < P$MinVow+P$MinCon)){
    stop("The issue is before the horizontal code.")
  }
    for(i in 1:repeats){
      Transfers <- PopulationPool[which(runif(length(PopulationPool))<P$HorzRate)]
      if(length(Transfers) > 0){
        S$Languages[Transfers,] <- t(sapply(1:length(Transfers),
                                            function(x)
                                              HorizontalTransfer(P, S$Languages,S$Local,
                                                                 S$PhonemeRelatedness,
                                                                 S$PhonemeProbab, Transfers[x]) ))
    }
  }
  return(S)
}

#' Horizontal Transfer
#'
#' A function wrapper that get the language to modify and allows the phoneme change to either add/shift or remove a phoneme if this can be done.
#' @param P A list of parameters.
#' @param languages All languages.
#' @param local The local territories data structure.
#' @param phonemeRelatedness The phoneme relatedness list.
#' @param phonemeProbab The probability of gaining each phoneme in the population.
#' @param index The target territory whose language may change.
#' @keywords Horizontal
#' @export
#'
HorizontalTransfer <- function(P, languages, local, phonemeRelatedness, phonemeProbab, index){
  TargetLanguage <- languages[index,]
  Type <- sample(2,1,prob=c(.6,.3))
  
  if(Type == 1){ #add/shift
    return(AddShift(P, TargetLanguage, languages, local, phonemeRelatedness, index))
  }else{  #Lose
    return(Lose(P, TargetLanguage, phonemeProbab))
  }
}

#' Add or Shift a Phoneme
#'
#' Allows a language to either gain a new phoneme or shift an existing phoneme to better match another population's phoneme iventory.
#' @param P A list of parameters.
#' @param targetLanguage The target language to be modified if possible.
#' @param languages All languages
#' @param local The local territories data structure.
#' @param phonemeProbab The probability of gaining each phoneme in the population.
#' @param index The target territory whose language may change.
#' @keywords Horizontal
#' @export
#'
AddShift <- function(P, targetLanguage, languages, local, phonemeRelatedness, index){
  #get potential syls
  if(P$Horilocal){
    SylProb <- colSums(languages[unlist(local[index]),])
  }else{
    SylProb <- colSums(languages[sample((1:P$nTerr)[-index],P$NumHori),])
  }
  
  LocalSyls <- which(SylProb!=0)
  if(length(LocalSyls) == 0){#no neighbors with a language
    return(targetLanguage)
  }
  New <- which(targetLanguage[LocalSyls] == 0)
  
  #pick a syl
  if(length(New)==0){
    return(targetLanguage)
  }else if(length(New)==1){
    Chosen <- New
  }else{
    Chosen <- sample(LocalSyls[New],1,prob=SylProb[LocalSyls[New]]) 
  }
  
  
  Switchable <- which(targetLanguage[phonemeRelatedness[[Chosen]][,1]] == 1)
  
  #potentially switch
  if(length(Switchable)!=0){
    if(runif(1)>.5){ #actually switch
      if(length(Switchable)==1){
        Switch <- phonemeRelatedness[[Chosen]][Switchable,1]
      }else{
        Switch <- sample(phonemeRelatedness[[Chosen]][Switchable,1],1,
                         prob=phonemeRelatedness[[Chosen]][Switchable,2])
      }
      targetLanguage[c(Chosen,Switch)] <- c(1,0)
      return(targetLanguage)
    }
  }
  
  
  #Add  
  if(sum(targetLanguage)==P$AvePho[3]){#maxed out
    return(targetLanguage)
  }
  
  targetLanguage[Chosen] <- 1
  return(targetLanguage)
}

#' Lose a Phoneme
#'
#' Allows a language to either lose a phoneme to better match other populations.
#' @param P A list of parameters.
#' @param targetLanguage The target language to be modified if possible.
#' @param phonemeProbab The probability of gaining each phoneme in the population.
#' @keywords Horizontal
#' @export
#'
Lose <- function(P, targetLanguage, phonemeProbab){

  Phonemes <- which(targetLanguage == 1)
  if(length(Phonemes) < P$MinVow+P$MinCon){
    stop("The issue is in the horizontal code.")
  }
  if(length(Phonemes) == P$MinVow+P$MinCon){#minned
    return(targetLanguage)
  }
  #which can be lost; vowels cons or both?
  if(length(which(Phonemes <= P$nConse)) == P$MinCon){#if min conned
    index <- which(Phonemes > P$nConse)
  }else if(length(which(Phonemes > P$nConse)) == P$MinVow){#if min voweled
    index <- which(Phonemes <= P$nConse)
  }else{index <- 1:length(Phonemes)}

  
  #pick a loser and lose it
  if(length(index) > 0){
    if(length(Phonemes[index]) == 1){
      Change <- Phonemes[index]
    }else{
      Change <- sample(Phonemes[index],1,prob=1-phonemeProbab[Phonemes[index]])
    }
    
    targetLanguage[Change] <- 0
  }else{stop("Someone has gone Terribly, Terribly WRONG and there is a bug.")}
  return(targetLanguage)
}


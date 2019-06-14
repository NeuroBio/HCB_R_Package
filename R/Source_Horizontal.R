#Allow for horizontal transfer
#'
#'
#' After migration, allow populations to exchnage phoneme information, losing or gaining syllables based on other populations in the simulation.
#' @param P A list of parameters.
#' @param S A list of the data structures.
#' @keywords Horizontal
#' @export
#'
HoritontalTransferRepeater <- function(P, S){
  PopulationPool <- which(S$OccupiedVacant)
    for(i in 1:P$Hsims){
      Transfers <- PopulationPool[which(runif(length(PopulationPool))<P$HorzRate)]
      S$Languages[Transfers,] <- t(sapply(1:length(Transfers),
                                        function(x)
                                        HorizontalTransfer(P, S$Languages,S$Local,
                                                           S$PhonemeRelatedness,
                                                          S$PhonemeProbab, Transfers[x]) ))
    } 
  
    print("Horizontal Transfer Finished")
    return(list(Populations=S$Populations, Languages=S$Languages,
                PhonemeProbab=S$PhonemeProbab))
}

#' Horizontal Transfer
#'
#' A function wrapper that get the language to modify and allows the phoneme change to either add/shift or remove a phoneme if this can be done.
#' @param P A list of parameters.
#' @param languages All languages.
#' @param local Locality.
#' @param phonemeRelatedness The phoneme relatedness list.
#' @param phonemeProbab The probability of gaining each phoneme in the population.
#' @param index The target territory whose language may change.
#' @keywords Horizontal
#' @export
#'
HorizontalTransfer <- function(P, languages, local, phonemeRelatedness, phonemeProbab, index){
  Language <- languages[index,]
  Type <- sample(2,1,prob=c(.6,.3))
  
  if(Type == 1){ #add/shift
    return(AddShift(P, Language, languages, local, phonemeRelatedness, index))
  }else{  #Lose
    return(Lose(P, Language, phonemeProbab))
  }
}

#' Add Shift Phoneme
#'
#' Allows a language to either gain a new phoneme and shift and existing phoneme to match another population.
#' @param P A list of parameters.
#' @param language The target language to be modified if possible.
#' @param languages All languages
#' @param local Locality.
#' @param phonemeProbab The probability of gaining each phoneme in the population.
#' @param index The target territory whose language may change.
#' @keywords Horizontal
#' @export
#'
AddShift <- function(P, language, languages, local, phonemeRelatedness, index){
  #get potential syls
  if(P$Horilocal){
    SylProb <- colSums(languages[unlist(local[index]),])
  }else{
    SylProb <- colSums(languages[sample((1:P$nTerr)[-index],P$NumHori),])
  }
  
  LocalSyls <- which(SylProb!=0)
  if(length(LocalSyls) == 0){#no neighbors with a language
    return(language)
  }
  New <- which(language[LocalSyls] == 0)
  
  #pick a syl
  if(length(New)==0){
    return(language)
  }else if(length(New)==1){
    Chosen <- New
  }else{
    Chosen <- sample(LocalSyls[New],1,prob=SylProb[LocalSyls[New]]) 
  }
  
  
  Switchable <- which(language[phonemeRelatedness[[Chosen]][,1]] == 1)
  
  #potentially switch
  if(length(Switchable)!=0){
    if(runif(1)>.5){ #actually switch
      if(length(Switchable)==1){
        Switch <- phonemeRelatedness[[Chosen]][Switchable,1]
      }else{
        Switch <- sample(phonemeRelatedness[[Chosen]][Switchable,1],1,
                         prob=phonemeRelatedness[[Chosen]][Switchable,2])
      }
      language[c(Chosen,Switch)] <- c(1,0)
      return(language)
    }
  }
  
  
  #Add  
  if(sum(language)==P$AvePho[3]){#maxed out
    return(language)
  }
  
  language[Chosen] <- 1
  return(language)
}

#' Lose Phoneme
#'
#' Allows a language to either lose a phoneme to better match other populations.
#' @param P A list of parameters.
#' @param language The target language to be modified if possible.
#' @param phonemeProbab The probability of gaining each phoneme in the population.
#' @keywords Horizontal
#' @export
#'
Lose <- function(P, language, phonemeProbab){

  Phonemes <- which(language == 1)
  if(length(Phonemes) == P$MinVow+P$MinCon){#minned
    return(language)
  }
  #which can be lost; vowels cons or both?
  if(length(Phonemes[which(Phonemes<=P$nConse)]) == P$MinCon){#if min conned
    index <- which(Phonemes > P$nConse)
  }else if(length(Phonemes[which(Phonemes>P$nConse)]) == P$MinVow){#if min voweled
    index <- which(Phonemes <= P$nConse)
  }else{index <- 1:length(Phonemes)}

  
  #pick a loser and lose it
  if(length(index) > 0){
    if(length(Phonemes[index]) == 1){
      Change <- Phonemes[index]
    }else{
      Change <- sample(Phonemes[index],1,prob=1-phonemeProbab[Phonemes[index]])
    }
    
    language[Change] <- 0
  }else{stop("Someone has gone Terribly, Terribly WRONG and there is a bug.")}
  return(language)
}


#' Migration
#'
#' Main simulation function.  Allows populations to migrate, split, and die.
#' @param P A list of parameters.
#' @param S A list of data structures.
#' @keywords SimParam
#' @export
#'
Migration <- function(P, S){
  n <- 0
  repeat{
    if(n == P$MSims){#if there are no more places to Migrate
      print("Migration Completed")
      Final <- list(S$Populations, S$Languages, S$OccupiedVacant)
      break()
    }
    
    if(P$Death){#kill off depending on popsize
      Dead <- Extinction(S$Populations, S$OccupiedVacant)
      if(length(Dead) > 0){
        S <- updateStructuresRemove(S, Dead)
      }  
    }
    
    #allow for growth and migrate
    S$Populations$SizeCurrent <- PopulationGrowth(P, S$Populations$SizeCurrent, which(S$OccupiedVacant))
    M <- GetImmigrants(P, which(S$OccupiedVacant), S$Local, S$Populations)
    if(is.null(M)){next()}#no immigrants
    
    
    #create new populations and languages
    if(length(M$SourcePop) > 0){
      Pops <- sapply(1:length(M$newTerr),
                     function(x) MakePopulation(P, M$SourcePop[x], S$Populations))
      for(i in 1:length(M$newTerr)){
        S$Populations[M$newTerr[i],] <- Pops[,i]
      }
      S$Languages[M$newTerr,] <- t(sapply(1:length(M$newTerr), function(x)
        MakeLanguage(P, S$PhonemeProbab, S$PhonemeRelatedness,
                     S$Languages[M$SourcePop[x],],
                     S$Populations$SizeCurrent[M$SourcePop[x]])))
    }
    
    #Move uprooted populations 
    if(length(M$UpRoot) > 0){
      S <- updateStructuresMove(S, M$UnewTerr, M$UpRoot)
      S <- updateStructuresRemove(S, M$UpRoot)
    }
    
    #update for next round
    S$OccupiedVacant[M$newTerr] <- TRUE
    n <- n+1
    if(P$UsePopSize || P$UpRoot){
      S$Populations$SizeCurrent[M$SourcePop] <- S$Populations$SizeCurrent[M$SourcePop]-S$Populations$SizeCurrent[M$newTerr]  
    }
    if(P$PhoProbT == "Frequency"){
      S$PhonemeProbab <- colSums(S$Languages)/(length(which(S$OccupiedVacant)))
    }
  }
  return(S)
}

#' Next Wave
#'
#' Adds the seed data for the next wave to the population and language dataframes.
#' @param P A list of parameters.
#' @param S A list of data structures.
#' @param i The number of the next wave.
#' @keywords SimParam
#' @export
#'
NextWave <-function(P, S, i){
  S$Populations[P$PopStart[i],] <- S$Waves$SeedPopulations[i,] 
  S$Languages[P$PopStart[i],] <- S$Waves$SeedLanguages[i,]
  S$OccupiedVacant[P$PopStart[i]] <- TRUE
  if(P$PhoProbT == "Frequency"){
    S$PhonemeProbab <- colSums(S$Languages)/(length(which(!is.na(S$Populations$Founder))))
  }
  return(S)
}

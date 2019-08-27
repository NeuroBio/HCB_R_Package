#' Migration
#'
#' Main simulation function.  Allows populations to migrate, split, and die.
#' @param P A list of parameters.
#' @param S A list of data structures.
#' @param repeats how many times to repeat migration.
#' @keywords SimParam
#' @export
#'
Migration <- function(P, S, repeats){
  n <- 0
  repeat{
    if(n == repeats){#if all runs complete
      Final <- list(S$Populations, S$Languages, S$OccupiedVacant)
      break()
    }

    if(P$Death){#kill off depending on popsize
      Dead <- Extinction(S$Populations, S$OccupiedVacant)
      if(length(Dead) > 0){
        S <- UpdateStructuresRemove(S, Dead)
      }
      print(length(which(S$OccupiedVacant)))
      if(length(which(S$OccupiedVacant)) == 0){
        stop("Everyone is dead.  Revisit your growth parameters.")
      }
    }
    
    #allow for growth and migrate
    GrowthList <- PopulationGrowth(P, S$Populations$SizeCurrent, which(S$OccupiedVacant))
    S$Populations$SizeCurrent <- GrowthList$Size
    S$OccuiedVacant <- GrowthList$Occupied
    M <- GetImmigrants(P, which(S$OccupiedVacant), S$Local, S$Populations)
    if(is.null(M)){next()}#no immigrants
    
    
    #create new populations and languages
    if(length(M$SourcePop) > 0){
      NewPopulations <- sapply(1:length(M$NewTerr),
                     function(x) MakePopulation(P, S$Populations[M$SourcePop[x],]) )
      for(i in 1:length(M$NewTerr)){
        S$Populations[M$NewTerr[i],] <- NewPopulations[,i]
      }
      S$Languages[M$NewTerr,] <- t(sapply(1:length(M$NewTerr), function(x)
        MakeLanguage(P, S$PhonemeProbab, S$PhonemeRelatedness,
                     S$Languages[M$SourcePop[x],],
                     S$Populations$SizeCurrent[M$SourcePop[x]], P$MutRat)))
    }
    
    #allow for mutatiosn in stationary populations
    Stationary <- which(S$OccupiedVacant)
    Stationary <- Stationary[-which(Stationary %in% M$UpRoot)]
    if(length(Stationary)>0){
      S$Languages[Stationary,] <- t(sapply(Stationary, function(x)
        MakeLanguage(P, S$PhonemeProbab, S$PhonemeRelatedness,
                     S$Languages[x,],
                     S$Populations$SizeCurrent[x], P$StateMRate)))      
    }
    
    #Move uprooted populations 
    if(length(M$UpRoot) > 0){
      S <- UpdateStructuresMove(S, M$UNewTerr, M$UpRoot)
      S <- UpdateStructuresRemove(S, M$UpRoot)
      S$Languages[M$UpRoot,] <- t(sapply(M$UpRoot, function(x)
        MakeLanguage(P, S$PhonemeProbab, S$PhonemeRelatedness,
                     S$Languages[x,],
                     S$Populations$SizeCurrent[x], P$UpRootMRate)))
    }
    
    #update for next round
    S$OccupiedVacant[M$NewTerr] <- TRUE
    n <- n+1
    if(P$UsePopSize || P$UpRoot){
      S$Populations$SizeCurrent[M$SourcePop] <- S$Populations$SizeCurrent[M$SourcePop]-S$Populations$SizeCurrent[M$NewTerr]  
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

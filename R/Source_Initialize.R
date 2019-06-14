#' Initialize
#'
#' The function wrapper that makes calls to create the population and phoneme data structures and then populatas them with initial data.
#' @param P A list of parameters.
#' @keywords Wrapper
#' @export
#'
Initialize <- function(P){
  #create a vector of probabilties to know each phoneme and how closely related phonemes are
  #create the seed languages
  PhonemeProbab <- GeneratePhonemeProbabilities(P)
  PhonemeRelatedness <- c(ShiftDirections(P$nConse),
                          ShiftDirections(P$nVowl, P$nConse))
  Languages <- matrix(0, ncol=P$nPhon, nrow=P$nTerr)
  SeedLanguages <- matrix(sapply(1:P$nPop,function(x)
                                  GenerateSeedLanguage(P, PhonemeProbab, x)),
                                  ncol=P$nPhon, byrow=TRUE)

  #Create the ppulation data structure and define which territories are local (implements barriers)
  #Create the population seed data and mark wich territories are occupied
  Populations <- data.frame(Founder=rep(NA,P$nTerr),
                            ID=rep("",P$nTerr),
                            SizeStart=numeric(P$nTerr),
                            SizeCurrent=numeric(P$nTerr),
                            SeedID=rep(NA,P$nTerr),
                            stringsAsFactors = FALSE)
  Local <- StepDirections(P)
  SeedPopulations <- cbind.data.frame(rep(0,P$nPop),
                                      sapply(rep(TRUE, P$nPop), UUIDgenerate),
                                      rep(P$PopSizeInfo[1], P$nPop),
                                      rep(P$PopSizeInfo[1], P$nPop),
                                      1:P$nPop,
                                      stringsAsFactors=FALSE)
  OccupiedVacant <- rep(FALSE,P$nTerr)
  
  if(P$Wave){#only add the first seed population
    Waves <- list(SeedLanguages=SeedLanguages, SeedPopulations=SeedPopulations)
    Languages[P$PopStart[1],] <- SeedLanguages[1,]
    Populations[P$PopStart[1],] <- SeedPopulations[1,]
    OccupiedVacant[P$PopStart[1]] <- TRUE
  }else{#add all seed populations
    Waves = NULL
    Languages[P$PopStart,] <- SeedLanguages
    Populations[P$PopStart,] <- SeedPopulations
    OccupiedVacant[P$PopStart] <- TRUE
  }

  
  if(P$PhoProbT == "Frequency"){#update the probability of knowing each phoneme
    PhonemeProbab <- colSums(Languages)/(length(which(!is.na(Populations$Founder))))
  }
  
  return(list(PhonemeProbab=PhonemeProbab, PhonemeRelatedness=PhonemeRelatedness,
              Local=Local, Languages=Languages, Populations=Populations,
              OccupiedVacant=OccupiedVacant, Waves=Waves))
}

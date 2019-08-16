#Generate new populations

#'Population Growth 
#'
#' Adds new individuals to existing populations when population size is used in the simulation.
#' @param P A list of parameters.
#' @param populationSizes The number of people live on each territory.
#' @param occupied The indicies of territories with people living on them.
#' @keywords Population_Dynamics
#' @export
#'
PopulationGrowth <- function(P, populationSizes, occupied){
  if(P$GrwMax == P$GrwMin){
    if(P$GrwMax %% 1 == 0){
      populationSizes[occupied] <- populationSizes[occupied]+P$GrwMax
    }else{
      populationSizes[occupied] <- populationSizes[occupied]*P$GrwMax
    }
  }else{
    if((P$GrwMax%%1 == 0) && (P$GrwMin%%1 == 0)){ #integer growth
      Change <- sample(P$GrwMin:P$GrwMax, length(occupied), replace = TRUE)
      populationSizes[occupied] <- populationSizes[occupied]+Change
    }else{ #percentage growth
      Change <- runif(length(occupied), max=P$GrwMax, min=P$GrwMin)
      populationSizes[occupied] <- populationSizes[occupied]*Change
    }
  }
  
  return(populationSizes)
}

#' Emigrate
#'
#' Picks which populations migrate, whether the entire population migrates or a founder party is sent off, and where the population migrates to.  Allows only one population to enter a territory.  When multiple populations attempt to enter the same territory, one is randomly chosen to do so while the rest stay put.
#' @param P A list of parameters.
#' @param occupied The territories with a population on them.
#' @param local The local territories data structure.
#' @param populations The data for all exising populations.
#' @keywords Population_Dynamics
#' @export
#'
Emigrate <- function(P, occupied, local, populations){
  #get the travelers
  Travelers <- occupied[which(runif(length(occupied))<P$ChExp)]
  if(length(Travelers)==0){
    return(NULL)
  }

  #allow whole population to move, or only those with a large enough popsize  
  UpRoot <- numeric()
  if(P$UpRoot){
    ChancetoUproot <- (P$PopSizeInfo[2]+P$PopSizeInfo[3]-1)/(populations$SizeCurrent[Travelers])
    UpRoot <- Travelers[which(runif(length(ChancetoUproot)) < ChancetoUproot)]
  }else if(P$UsePopSize){
    Travelers <- Travelers[which(populations$SizeCurrent[Travelers]>=(P$PopSizeInfo[2]+P$PopSizeInfo[3]))]
  }
  
  #pick new territory
  NewTerr <- rep(0,length(Travelers))
  for(i in seq_along(Travelers)){
    LocalTerritory <- local[[Travelers[i]]]
    OpenTerritory <- which(is.na(populations[LocalTerritory, 1]))
    NewTerr[i] <- GetTerritory(LocalTerritory, OpenTerritory)
  }
  
  #Only allow one pop to enter a territory
  #if multiple attempt, randomly sample for one to succeed
  Duplicates <- unique(NewTerr[duplicated(NewTerr, incomparables = c(NA, 0))])
  if(length(Duplicates != 0)){
    for(i in seq_along(Duplicates)){
      DupIndex <- which(NewTerr == Duplicates[i])
      save <- sample(DupIndex, 1)
      NewTerr[DupIndex[-save]] <- 0
    }
  }
  
  return(list(SourcePop=Travelers, NewTerr=NewTerr, UpRoot=UpRoot))
}

#' Get Territory
#'
#' Returns a territory a population can migrate to or NA if none are available.
#' @param local Territories that are within reach of the target territory.
#' @param open Which territoies can be migrated to (i.e. no other population currently resides there).
#' @keywords Population_Dynamics
#' @export
#'
GetTerritory <- function(local, open){
  if(length(open)==0){#no open territories
    return(NA)
  }
  if(length(open)==1){#one open Territory
    return(local[open])
  }
  #multiple open territories
  return(local[[sample(open,1)]])
}

#' Reset Population
#'
#' Resets phoeneme and population data in the original territory when an entire population moes to a new territory.
#' @keywords Population_Dynamics
#' @export
#'
ResetPopulation <-function(){
  return(data.frame(NA,"",numeric(1), numeric(1), NA,
                    stringsAsFactors=FALSE))
}

#' Extinction
#'
#' Tests which populations will die based on population size and random chance.
#' @param populations The data for all exising populations.
#' @param occupied The territories with a population on them.
#' @keywords Population_Dynamics
#' @export
#'
Extinction <- function(populations, occupied){
  Alive <- which(occupied)
  if(length(Alive) > 1){
    DeadIndex <- which(runif(length(Alive))< 1/populations$SizeCurrent[Alive])
    return(Alive[DeadIndex])
  }
  return(integer())
}

#' Update Structures Move
#'
#' Copies population data from one territory to another when the entire population migrates and then erases the original data.
#' @param S A list of data structures.
#' @param move The indicies of territories
#' @param former The indicies of territories
#' @keywords Population_Dynamics
#' @export
#'
UpdateStructuresMove <- function(S, move, former){
  S$OccupiedVacant[move] <- TRUE
  S$Populations[move,] <- S$Populations[former,]
  S$Languages[move,] <- S$Languages[former,]
  return(UpdateStructuresRemove(S, former))
}

#' Update Structures Remove
#'
#' Deletes population and language information for specified territories.
#' @param S A list of data structures.
#' @param Remove The indicies of territories whose data should be erased.
#' @keywords Population_Dynamics
#' @export
#'
UpdateStructuresRemove <- function(S, remove){
  S$OccupiedVacant[remove] <- FALSE
  for(i in seq_along(remove)){
    S$Populations[remove,] <- ResetPopulation()
  }
  S$Languages[remove,] <- matrix(0, ncol=ncol(S$Languages), nrow=length(remove))
  return(S)
}

#' Get Immigrants
#'
#' Tests which populations immigrate, removes those that did not migrate from the migraton data.  Splits the data into populations that sent off founder parties and those that moved as a single population.
#' @param P A list of parameters.
#' @param occupied The territories with a population on them.
#' @param local The local territories data structure.
#' @param populations The data for all exising populations.
#' @keywords Population_Dynamics
#' @export
#'
GetImmigrants <- function(P, occupied, local, populations){
  ImmigrantData <- Emigrate(P, occupied, local, populations)
  
  #no population attempted immigration
  if(is.null(ImmigrantData$NewTerr)){
    return(ImmigrantData)
  }
  
  #break into populations that moved as a whole versus those that budded
  UpRootedIndex <- which(ImmigrantData$SourcePop %in% ImmigrantData$UpRoot)
  if(length(UpRootedIndex) >0){
    ImmigrantData$SourcePop <- ImmigrantData$SourcePop[-UpRootedIndex]
    ImmigrantData$UNewTerr <- ImmigrantData$NewTerr[UpRootedIndex]
    ImmigrantData$NewTerr <- ImmigrantData$NewTerr[-UpRootedIndex]
  }
  
  #Remove those that failed to migrate
  FailedSource <- which(is.na(ImmigrantData$NewTerr) | (ImmigrantData$NewTerr == 0))
  if(length(FailedSource)!=0){
    ImmigrantData$SourcePop <- ImmigrantData$SourcePop[-FailedSource]
    ImmigrantData$NewTerr <- ImmigrantData$NewTerr[-FailedSource]
  }
  
  FailedRoot <- which(is.na(ImmigrantData$UNewTerr) | (ImmigrantData$UNewTerr == 0))
  if(length(FailedRoot)!=0){
    ImmigrantData$UpRoot <- ImmigrantData$UpRoot[-FailedRoot]
    ImmigrantData$UNewTerr <- ImmigrantData$UNewTerr[-FailedRoot]
  }
  
  return(ImmigrantData)
}

#' Make Population
#'
#' Generates new population based on the parent population.
#' @param P A list of parameters.
#' @param population The population data used to make a new population.
#' @keywords Population_Dynamics
#' @export
#'
MakePopulation <- function(P, population){
  if(P$UsePopSize){
    if(population$SizeCurrent==P$PopSizeInfo[2]+P$PopSizeInfo[3]){
      Size <- P$PopSizeInfo[2]
    }else{
      if(is.na(P$PopSizeInfo[4])){
        Max <- population$SizeCurrent
      }else{Max <- P$PopSizeInfo[4]}
      Size <- sample(P$PopSizeInfo[2]:Max,1)
    }
  }else{
    Size <- 0
  }
  return(cbind.data.frame(population$ID,
                          UUIDgenerate(TRUE),
                          Size, Size, population$SeedID, stringsAsFactors=FALSE))
}

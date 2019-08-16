#' Define Parameters
#'
#' Creates a parameter data structure for running simulations.
#' @param Rows The number of rows in the world matrix.
#' @param Cols The number of columns in the world matrix.
#' @param ChanceExpand The chance that a population will either move or send off a group of individuals to found a new population.
#' @param PopulationStartIndex The position in the matrix where each seed population starts.  The number of seed populations is defined by the number of starting indicies.
#' @param NumPopulationPhonemes The number of phonemes in each starting population.  If set to NA, this is decided by sampling from a distribution with min, mode, and made on the values from the PhonemeDistribution arguement.
#' @param UsePopSize Whether to take into account the the population size (number of people) when making decisions about moving, immegrating, the mutation rate ,and phoneme loss/addition biases.
#' @param IndividualsStEmSuEM Four related parameters: 1) The number of individuals a seed population starts with, 2) the minumum number of individuals required to make a founder party to settle a new territory, 3) the minumum number of individuals that must stay behind when a founder party is sent off, and 4) the maximum number of individuals allowed to be in one founder party.
#' @param MutationRate The rate at which phonemes mutate.  E.g., if MutationRate==0.1, each phoneme in a populatiosn phoneme inventory has a 10\% chance to mutate.  Note that when usePopSize==TRUE, this is better conceptualized as teh maximum mutation rate (larger populations have lower mutation rates).
#' @param PhonemeDistribution The 1) min, 2) mode, and 3) max number of phonemes a population can have when sampling for seed population sizes and when preventing languages from gaining or losing too many phonemes. Defaults based on real phoneme data.
#' @param Consonants The number of possible consonants in existence.  Default based on real phoneme data.
#' @param Vowels The number of possible vowels in existence. Default based on real phoneme data.
#' @param MinConsonant The minumum number a consonants that can be in a population's phoneme inventory. Default based on real phoneme data.
#' @param MinVowel The minumum number a vowels that can be in a population's phoneme inventory. Default based on real phoneme data.
#' @param PhonemeProbabilityType The method by which phoneme probabilities are established.  Can be Real (uses teh real data verbatim, and requires the correct number of cosonants and phonemes), RealMimic (uses teh real data to generate a new distribution of probability similar ot the real data, can be used with any number of phonemes), Equal (all phonemes are equally liekly to be known), Frequency (based on how common phonemes are across populations in teh simulation), or Random (randomly generated).
#' @param GrowthRate When an integer, the number of individuals added to each population every time step.  When a fraction, the percent that a population increases each timestep.
#' @param Barriers Whether to create "snake barriers" that limit the direction of migration in the matrix.
#' @param BarrierLength The width of snake barriers.
#' @param BarrierBreaks The height of the space between snake barriers.
#' @param MutationTypeChance The chance that each mutation type occurs.  1) Add, 2) Lose, 3) Split, 4) Join, and 5) Shift.
#' @param HorizontalRate The fraction of the population that attempts to modify its phoneme inventory every horizontal timestep.
#' @param Bias Whether to randomly bias mutations towards either  gains or losses when populations are small.  Set to true based on previously published data.
#' @param Steps The number of distance steps away from a target location that are considered "local."  Includes all 8 cardinal and ordinal directions around a target, so the local area is always a rectangle around the target location.
#' @param HorizontalSimSteps The number of time steps to spend on horizontal transfer.
#' @param HorizontalLocal Whether horizontal transfer occurs between local populations or globally.  Set to FALSE as a control, as global horizontal transfer should abolish local patterns.
#' @param NumberRandomHorizontal The number of locations to compare when HorizontalLocal==FALSE.  Should be 8 when Steps==1, 24 when steps==2, 48 when Steps=3, ect.
#' @param UpRoot Whether established populations can move (TRUE) or they remain in place for the entire simulation (FALSE).
#' @param Death Whether populations can die out.
#' @param Bering Whether to employ barriers that mimick the Bering Strait and Americas.
#' @param MigrationSimSteps The number of time steps to run each wave of migration.
#' @param BeringLength An integer of length 0 to 24 that degines how long the berring straight is
#' @param Waves Whether migration occurs in waves or all seed populations are added at the same time.  If TRUE, there is one wave for each seed population.
#' @param Seed Sets a seed for reproducibility if an integer instead of NA.
#' @keywords SimParam
#' @export
DefineParameters <- function(Rows=40, Cols=50, ChanceExpand=.8, PopulationStartIndex=c(1,2),
                             NumPopulationPhonemes=rep(NA, length(PopulationStartIndex)),
                             UsePopSize=TRUE,IndividualsStEmSuEM=c(1000,10,20,NA),
                             MutationRate=.15, PhonemeDitribution=c(12,24,133),
                             Consonants=750, Vowels=100, MinConsonant=6, MinVowel=6,
                             PhonemeProbabilityType="RealMimic",
                             GrowthRate=5, Barriers = FALSE, BarrierLength=30, BarrierBreaks=4,
                             MutationTypeChance=rep(1/5,5), HorizontalRate=.1, Bias=TRUE,
                             Steps=1, HorizontalLocal=TRUE, NumberRandomHorizontal=8,
                             UpRoot=TRUE, Death=TRUE, Bering=FALSE, BeringLength=20,
                             MigrationSimSteps=300,
                             HorizontalSimSteps=400, Waves=FALSE, Seed=NA){
  if(BeringLength > 24 || BeringLength < 0 || BeringLength %% 1 != 0){
    stop("BeringLength must be an integer between 0 and 24.")
  }
  if(PhonemeProbabilityType == "Real"){
    Consonants <- 604
    Vowels <- 67
  }
  Params <- list(R=Rows, C=Cols, nTerr=Rows*Cols, ChExp=ChanceExpand,
                 nPop=length(PopulationStartIndex), PopStart=PopulationStartIndex,
                 NumPopPho=NumPopulationPhonemes, UsePopSize=UsePopSize,
                 PopSizeInfo=IndividualsStEmSuEM, GrwRt=GrowthRate,
                 MutRat=MutationRate, HorzRate=HorizontalRate, AvePho=PhonemeDitribution,
                 nPhon=Consonants+Vowels, nConse=Consonants, nVowl=Vowels,
                 MinVow=MinVowel, MinCon=MinConsonant, Bias=Bias, NumHori=NumberRandomHorizontal,
                 PhoProbT = PhonemeProbabilityType,Horilocal=HorizontalLocal,
                 Bar = Barriers, BarLen=BarrierLength, BarBre=BarrierBreaks,
                 MutTChan=MutationTypeChance, Steps=Steps, HSims=HorizontalSimSteps,
                 UpRoot=UpRoot, Death=Death, MSims=MigrationSimSteps, Bering=Bering,
                 Waves=Waves, BSLength=BeringLength,
                 Seed=Seed)
  return(Params)
}


#' Human Cultural Boundaries Simulation
#'
#' Runs a simulation.
#' @param P A list of parameters.
#' @keywords SimParam
#' @export
#'

HCBSimmulation <- function(P){
  #throughout the code, P=Parameters, S=data Structure
  if(!is.na(P$Seed)){ set.seed(P$Seed) }
  S <- Initialize(P) #create data structures
  if(P$Waves){
    for(i in 2:(P$nPop)) {
      S <- Migration(P, S, P$MSims)
      S <- NextWave(P, S, i)
      print(paste0("Wave ", i-1, " Completed"))
    }
  }
  
  #populate the matrix
  S <- Migration(P, S, P$MSims)
  Pre <- S
  print("Migration Completed")
  
  #Horzontal transfer
  if(P$HSims > 0){
    Post <- HoritontalTransferRepeater(P, S, P$HSims)
    print("Horizontal Transfer Finished")
    return(list(NoHorizontal=Pre, Horizontal=Post))
  }
  return(list(NoHorizontal=Pre))
}

HCBAlternatorSimmulation <- function(P){
  #throughout the code, P=Parameters, S=data Structure
  if(!is.na(P$Seed)){ set.seed(P$Seed) }
  S <- Initialize(P) #create data structures
  if(P$Waves){
    for(i in 2:(P$nPop)) {
      S <- Alternator(P, S, P$MSims)
      S <- NextWave(P, S, i)
      print(paste0("Wave ", i-1, " Completed"))
    }
  }
  
  S <- Alternator(P, S, P$MSims)
  print("Migration Completed")
  if(P$HSims > 0){
      Post <- HoritontalTransferRepeater(P, S, P$HSims)
      print("Horizontal Transfer Finished")
  }
  return(list(Alternated=S))
}


#' Migration
#'
#' A function wrapper that alternates between migration and horizontal transfer.
#' @param P A list of parameters.
#' @param S A list of data structures.
#' @param repeats how many times to repeat migration.
#' @keywords SimParam
#' @export
#'
Alternator <- function(P, S, repeats){
  for(i in 1:repeats){
    S <- Migration(P, S, 1)
    S <- HoritontalTransferRepeater(P, S, 1) 
  }
  return(S)
}
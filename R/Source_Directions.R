#direction/location code
#' Shift Directions
#'
#' Returns the relationships between phonemes with an offset of start.
#' @param nPhonemes The number of phonemes.
#' @param start Where to start number the phonemes (0 for consonants, number of consonants +1 for vowels).
#' @keywords Directions
#' @export
#'
ShiftDirections <- function(nPhonemes, start=0){
  RC <- GetFactorDim(nPhonemes)
  if(RC[1]==1){ #one-dimentional phoneme evolution
    PhonemeShifts <- as.list(1:nPhonemes)
    PhonemeShifts[[1]] <- cbind(c(2,3)+start,c(.2,.025))
    PhonemeShifts[[2]] <- cbind(c(1,3,4)+start,c(.2,.2,.025))
    for(i in 3:(nPhonemes-2)){
      PhonemeShifts[[i]] <- cbind(c(i-1,i+1,i-2,i+2)+start, c(.2,.2,.025,.025))  
    }
    PhonemeShifts[[nPhonemes-1]] <- cbind(c(nPhonemes-2,nPhonemes,nPhonemes-3)+start,c(.2,.2,.025))
    PhonemeShifts[[nPhonemes]] <- cbind(c(nPhonemes-1,nPhonemes-2)+start,c(.2,.025))
  }else{ #two-dimentional phoneme evolution
    StepOne <- OneStepDirections(RC[1],RC[2], start, TRUE)
    StepTwo <- NextStepDirections(StepOne,StepOne, start)
    PhonemeShifts <- sapply(1:length(StepOne),
                            function(x) cbind(c(StepOne[[x]],StepTwo[[x]]),
                                              rep(c(.2,.025),c(length(StepOne[[x]]), length(StepTwo[[x]]))) ))
  }
  return(PhonemeShifts)
}

#' Step Directions
#'
#' A wrapper that calls StepOne(), add barriers if required, then expands StepOne as many steps as the Steps parameter calls for.
#' @param P A list of parameters.
#' @keywords Directions Wrapper
#' @export
#'
StepDirections <- function(P){
  StepOne <- OneStepDirections(P$R, P$C)
  if(P$Bering){
    StepOne <- AddBeringStrait(P,StepOne)
  }else if(P$Bar){
    StepOne <- AddSnakeBarriers(P,StepOne)
  }
  if(P$Steps == 1){
    return(StepOne)
  }
  CurrentStep <- StepOne
  for(i in 2:P$Steps){
    CurrentStep <- NextStepDirections(StepOne,CurrentStep) 
  }
  return(CurrentStep)
}

#' Next Step Directions
#' 
#' Expands the Steps list one more step out.
#' @param firstStep The original StepOne.
#' @param currentStep StepOne in its current state. 
#' @param start How much to offset numbers (for phoneme structures).
#' @keywords Directions
#' @export
#'
NextStepDirections <- function(firstStep, currentStep, start=0){
  #serially expand the steps away from a target cell by one by taking the
  #indicies from cells that are currently connected to the target cell 
  Index <- sapply(1:length(currentStep), function(x)
    unique(unlist(firstStep[currentStep[[x]]-start])))
  NextStep <- sapply(1:length(firstStep), function(x)
    Index[[x]][-which(Index[[x]] == x+start)])
  return(NextStep)
}

#' One Step Directions
#'
#' Creates the FirstStep data structure.
#' @param R The number of rows.
#' @param C The number of columns.
#' @param start How much to offset numbers (for phoneme structures).
#' @param round whether to make the spacing Round (Phonemes) or Sqaure (Territories).
#' @keywords Directions
#' @export
#'
OneStepDirections <- function(R, C, start=0, round=FALSE){
  CellDirections <- as.list(1:(R*C))
  
  Center <- matrix(1:((C-2)*(R-2)), nrow=(R-2), ncol=(C-2))
  for(i in 1:(C-2)){
    Center[,i] <- Center[,i] +(R+1+2*(i-1))}
  for(i in Center){
    CellDirections[[i]] <- CardinalDirections(i, R, start, round,
                                              South=TRUE, North=TRUE, East=TRUE, West=TRUE,
                                              SE=TRUE, NE=TRUE, SW=TRUE, NW=TRUE)}
  #West Edge
  for(i in 2:(R-1) ){
    CellDirections[[i]] <- CardinalDirections(i, R, start, round,
                                              South=TRUE, North=TRUE, East=TRUE, West=FALSE,
                                              SE=TRUE, NE=TRUE, SW=FALSE, NW=FALSE)}
  #South Edge
  for(i in R*(2:(C-1)) ){
    CellDirections[[i]] <- CardinalDirections(i, R, start, round,
                                              South=FALSE, North=TRUE, East=TRUE, West=TRUE,
                                              SE=FALSE, NE=TRUE, SW=FALSE, NW=TRUE)}
  #North Edge
  for(i in 1+R*(1:(C-2)) ){
    CellDirections[[i]] <- CardinalDirections(i, R, start, round,
                                              South=TRUE, North=FALSE, East=TRUE, West=TRUE,
                                              SE=TRUE, NE=FALSE, SW=TRUE, NW=FALSE)}
  #East Edge
  for(i in (R*(C-1)+2):(R*C-1) ){
    CellDirections[[i]] <- CardinalDirections(i, R, start, round,
                                              South=TRUE, North=TRUE, East=FALSE, West=TRUE,
                                              SE=FALSE, NE=FALSE, SW=TRUE, NW=TRUE)}
  #Corners
  #NW Corner
  CellDirections[[1]] <- CardinalDirections(1, R, start, round,
                                            South=TRUE, North=FALSE, East=TRUE, West=FALSE,
                                            SE=TRUE, NE=FALSE, SW=FALSE, NW=FALSE)
  #SW Corner
  CellDirections[[R]] <- CardinalDirections(R, R, start, round,
                                            South=FALSE, North=TRUE, East=TRUE, West=FALSE,
                                            SE=FALSE, NE=TRUE, SW=FALSE, NW=FALSE)
  #NE Corner
  CellDirections[[1+(R*(C-1))]] <- CardinalDirections(1+(R*(C-1)), R, start, round,
                                                      South=TRUE, North=FALSE, East=FALSE, West=TRUE,
                                                      SE=FALSE, NE=FALSE, SW=TRUE, NW=FALSE)
  #SE Corner
  CellDirections[[R*C]]  <- CardinalDirections(R*C, R, start, round,
                                               South=FALSE, North=TRUE, East=FALSE, West=TRUE,
                                               SE=FALSE, NE=FALSE, SW=FALSE, NW=TRUE)
  return(CellDirections)
}

#' Cardinal Directions
#'
#' Calculates the terrritory indicies of locations around a target territory (also used for phoneme relatedness in the same way).
#' @param target The territory around which to get local territoies.
#' @param R The number of rows.
#' @param start How much to offset numbers (for phoneme structures).
#' @param round Whether to get a "round" set of territories (N, S, E, W only) for phonemes or a square set of territories (includes diagonals) for distance.
#' @param south Whether to get the southern territory.
#' @param north Whether to get the northern territory.
#' @param east Whether to get the eastern territory.
#' @param west Whether to get the western territory.
#' @param SE Whether to get the southeasrern territory.
#' @param NE Whether to get the northestern territory.
#' @param SW Whether to get the southwestern territory.
#' @param NW Whether to get the northwestern territory.
#' @keywords Directions
#' @export
#'
CardinalDirections <- function(target, R, start, round, South, North, East, West, SE, NE, SW, NW){
  Directions <- list()
  if(South){Directions['south'] <- target+1}
  if(North){Directions['north'] <- target-1}
  if(East){Directions['east'] <- target+R}
  if(West){Directions['west'] <- target-R}
  if(!round){
    if(SE){Directions['se'] <- target+R+1}
    if(NE){Directions['ne'] <- target+R-1}
    if(SW){Directions['sw'] <- target-R+1}
    if(NW){Directions['nw'] <- target-R-1}
  }
  return(unlist(Directions)+start)
}

#' Get Factor Dimentions
#'
#' Given a number of consonants or vowels, creates a data structure that is as square as possible.
#' @param nPhonemes The number of Phonemes (vowels or consonants).
#' @keywords Directions
#' @export
#'
GetFactorDim <- function(nPhonemes){
  facts <- primeFactors(nPhonemes)
  if(length(facts) == 1){#Dial is prime
    warning("Number of phonemes should not be prime")
    return(c(1,nPhonemes))
    
  }else{#Dial is not prime
    First <- facts[1]
    Second <- facts[length(facts)]
    if(length(facts)>2){
      for(i in 2:(length(facts)-1)){
        if(facts[i]*First < facts[i]*Second){
          First <- facts[i]*First 
        }else{
          Second <- facts[i]*Second
        }
      }
    }
  }
  return(c(First,Second))
}

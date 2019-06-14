#barrier code
#barries are implemented by affacting the local data;
#Territories that are normally considered local to a target are moved from the target's
#local listing.  They expect to be called after OneStepDirections() and before nextstep directions

#' Add Snake Barriers
#'
#' Removes connections at the FirstStep stage of the Local structure to create "barriers" between cells.  Snake Barriers are lines with length and spacing defined by the parameters.  The barriers jut out from the east and west walls, alternating east, west, east, west.  This creates a snaking, zig-zag pattern, hence the name.
#' @param P A list of parameters.
#' @param firstStep The local directions created by OneStepDirections().
#' @keywords Barriers
#' @export
#'
AddSnakeBarriers <- function(P, firstStep){
  NumBar <- P$R/P$BarBre
  for(k in 1:floor(NumBar)){
    
    #barrier from the left
    for(i in 1:P$BarLen){
      index <- k*(P$BarBre)+P$R*(i-1)-floor(P$BarBre/2)
      firstStep <- RemoveHorizontalConnections(P$R, index, firstStep, right=(i != P$BarLen))
    }
    
    #barrier from the right
    if(NumBar%%1!=0 || k != floor(NumBar)){
      for(i in 1:P$BarLen){
        index <- P$R*(P$C-i)+P$BarBre*k
        firstStep <- RemoveHorizontalConnections(P$R, index, firstStep, left=(i != P$BarLen))
      }
    }
  }
  return(firstStep)
}

#' Add Bering Strait
#'
#' Removes connections at the FirstStep stage of the Local structure to create "barriers" between cells.  Bering Strait Barriers are designed to create structures similar to the Bering Strait entering North America, going through Central America, then opening up into South America.
#' @param P A list of parameters.
#' @param firstStep The local directions created by OneStepDirections().
#' @keywords Barriers
#' @export
#'
AddBeringStrait <- function(P, firstStep){
  AsiaLowerright <- P$R*20
  AsiaUpperright <- AsiaLowerright-15
  AsiaBearingCorner <- P$R*4-15
  BeringNAmericaCorner <- P$R*3 + 5
  NAmericanlowerright <- AsiaUpperright + P$R*18
  NAmericanLowerEntry <- NAmericanlowerright - 5
  NAmericanUpperright <- NAmericanLowerEntry%/%P$R*P$R+1
  
  for(i in AsiaLowerright:AsiaUpperright ){
    firstStep <- RemoveVerticalConnections(P$R, i, firstStep)
  }
  for(i in seq(AsiaUpperright,AsiaBearingCorner, by=-P$R)-1){
    firstStep <- RemoveHorizontalConnections(P$R, i, firstStep)
  }
  for(i in AsiaBearingCorner:BeringNAmericaCorner ){
    firstStep <- RemoveVerticalConnections(P$R, i, firstStep)
  }
  for(i in seq(AsiaUpperright, NAmericanlowerright, by=P$R)-1){
    firstStep <- RemoveHorizontalConnections(P$R, i, firstStep)
  }
  for(i in NAmericanLowerEntry:NAmericanUpperright ){
    firstStep <- RemoveVerticalConnections(P$R, i, firstStep)
  }
  return(firstStep)
}

#' Remove Horizontal Connections
#'
#' Affects local territories below/South (and perhaps to the Southeast and Southwest) the target territory (index) and above/North (perhaps Northwest and Northeast) of index +1.
#' @param R The number of rows in the population matrix.
#' @param index The target territory.
#' @param firstStep The local directions created by OneStepDirections().
#' @param right Whether to remove the right diagonal.
#' @param left Whether to remove the left diagonal.
#' @keywords Barriers
#' @export
#'
RemoveHorizontalConnections <- function (R, index, firstStep, right=TRUE, left=TRUE){
  #removes connetions BELOW index and ABOVE the index+1
  Removebelow <- which(firstStep[[index]] == index+1)
  Removeabove <- which(firstStep[[index+1]] == index)
  if(left){
    Removebelow <- c(Removebelow, which(firstStep[[index]] == index-R+1))
    Removeabove <- c(Removeabove, which(firstStep[[index+1]] == index-R))
  }
  if(right){
    Removebelow <- c(Removebelow, which(firstStep[[index]] == index+R+1))
    Removeabove <- c(Removeabove, which(firstStep[[index+1]] == index+R))
  }
  firstStep[[index]] <- firstStep[[index]][-Removebelow]
  firstStep[[index+1]] <- firstStep[[index+1]][-Removeabove]
  return(firstStep)
}

#' Remove Vertical Connections
#'
#' Affects local territories right/East (and perhaps to the Northeast and Southeast) the target territory (index) and left/West (perhaps Northwest and Southwest) of index + R.
#' @param R The number of rows in the population matrix.
#' @param index The target territory.
#' @param firstStep The local directions created by OneStepDirections().
#' @param above Whether to remove the upper diagonal.
#' @param below Whether to remove the lower diagonal.
#' @keywords Barriers
#' @export
#'
RemoveVerticalConnections <- function (R, index, firstStep, above=TRUE, below=TRUE){
  #removes connetions RIGHT index and LEFT of index+R
  
  Removeright <- which(firstStep[[index]] == index+R)
  Removeleft <- which(firstStep[[index+R]] == index)
  
  if(below){
    Removeright <- c(Removeright, which(firstStep[[index]] == index+R+1))
    Removeleft <- c(Removeleft, which(firstStep[[index+R]] == index+1))
  }
  if(above){
    Removeright <- c(Removeright, which(firstStep[[index]] == index+R-1))
    Removeleft <- c(Removeleft, which(firstStep[[index+R]] == index-1))
  }
  firstStep[[index]] <- firstStep[[index]][-Removeright]
  firstStep[[index+R]] <- firstStep[[index+R]][-Removeleft]
  return(firstStep)
}

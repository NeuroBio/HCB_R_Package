#Plotting

#THIS NEEDS TO BE CLEANED UP

#' Get Groups
#'
#' Returns the territories descended from each seed.  Includes detailed ancestory data.  Ony works when Uproot and Death are FALSE.
#' @param P A list of parameters.
#' @param Data The Pre or Post output from an HBC simulation.
#' @keywords Plotting
#' @export
#'
GetGroups <- function(P, Data){
  Groups <- list()
  for(i in seq_along(P$PopStart)){
    G <- list()
    start <- P$PopStart[i]
    repeat{#given a starting place, what populations did it create?
      start <- which(Data$Populations[,1] %in% Data$Populations[start,2])
      if(length(start)==0){
        break()
      }
      G[[length(G)+1]] <- start
    }
    if(length(G)==0){
      Terr <- NA
      Connect <- NA
    }else{
      Terr <- sort(unlist(G))
      Connect <- sapply(1:length(Terr), function(x) which(Data$Populations[,2] == Data$Populations[Terr[x],1])) 
    }
    Groups[[i]] <- cbind(Terr,Connect)
  }
  return(Groups)
}

#' Bering Straight Plot
#'
#' Creates a plot that shows the Bring strait boundaries.
#' @param P A list of parameters.
#' @param Data The Pre or Post output from an HBC simulation.
#' @keywords Plotting
#' @export
#
BeringStraitPlot <- function(P, Data){
  Groups <- GroupBySeed(P, Data)
  colors <- randomColor(length(Groups))
  par(mar=c(3,2.5,1,1), mgp=c(1.5,.5,0), mfrow=c(2,2), bg="grey10", fg="white")
  hist(Data$Populations$SizeCurrent,
       xlab="Population Size", ylab="Number of Populations",
        col.axis="white", col.lab="white", col = "lightblue", border="lightblue4")
  PopulationPlot(P, Groups, colors)
  PhonemePopulationFrequencyPlots(P, Data, Groups, colors, sort=TRUE)
  }

#' Group By Seed
#'
#' Returns the territories descended from each seed.  Works for all sims, but lacks ancestory data.
#' @param P A list of parameters.
#' @param Data The Pre or Post output from an HBC simulation.
#' @keywords Plotting
#' @export
#
GroupBySeed <- function(P, Data){
  Groups <- vector("list")
  for(i in seq_along(P$PopStart)){
    Groups[[i]] <- which(Data$Populations$SeedID == i)
  }
  return(Groups)
}

#' Snapshot Plot
#'
#' Shows which territories are populated and from which seed they decended.
#' @param P A list of parameters.
#' @param Data The Pre or Post output from an HBC simulation.
#' @param colors A vector of colors, one for each seed.
#' @keywords Plotting
#' @export
#
SnapshotPlot<-function(P, Data, colors){
  Groups <- GroupBySeed(P, Data)
  PopulationPlot(P, Groups, colors)
}

#' Population Plot
#'
#'
#' @param P A list of parameters.
#' @param seedGroups Group structure of which territories were descended from what population seed.
#' @param colors A vector of colors, one for each seed.

#' @keywords Plotting
#' @export
#
PopulationPlot <- function(P, seedGroups, colors){
  plot(0, type="n", xlim=c(1, P$C), ylim=c(P$R,1),
       col.axis="white", font.axis=2)
  for(i in seq_along(seedGroups)){
    if(length(seedGroups[[i]]) > 0){
      Modtest <- seedGroups[[i]]%%P$R
      points(ceiling(seedGroups[[i]]/P$R), ifelse(Modtest==0,P$R,Modtest), col=colors[i], pch=19)
    }
  }
  if(P$Bering){
    AsiaLowerRight <- P$R*20
    AsiaUpperRight <- AsiaLowerRight-15-1
    AsiaBeringCorner <- P$R*4-15-1
    BeringNAmericaCorner <- P$R*3 + 5
    NAmericanlowerRight <- AsiaUpperRight + P$R*18
    NAmericanLowerEntry <- NAmericanlowerRight - 5
    NAmericanUpperRight <- NAmericanLowerEntry%/%P$R*P$R+1
    
    AddSegment(P, AsiaLowerRight, AsiaUpperRight)
    AddSegment(P, AsiaBeringCorner, NAmericanlowerRight)
    AddSegment(P, AsiaBeringCorner, BeringNAmericaCorner)
    AddSegment(P, NAmericanLowerEntry, NAmericanUpperRight, top=TRUE) 
  }
}

#' Add Segment
#'
#' Creates lines to show the Bering Straight borders.
#' @param P A list of parameters.
#' @param Data The Pre or Post output from an HBC simulation.
#' @keywords Plotting
#' @export
#
AddSegment<- function(P,a,b, top=FALSE){
  Modtest1 <- a%%P$R
  Modtest2 <- b%%P$R
  if(top){
    segments(ceiling(a/P$R)+.5, ifelse(Modtest1==0,P$R,Modtest1)+.5,
             ceiling(b/P$R)+.5, Modtest2-.5,
             col="white", lwd=2)
  }else{
    segments(ceiling(a/P$R)+.5, ifelse(Modtest1==0,P$R,Modtest1)+.5,
             ceiling(b/P$R)+.5, ifelse(Modtest2==0,P$R,Modtest2)+.5,
             col="white", lwd=2)
  }
}

#' Migration Plot
#'
#' Shows the expansion of populations from the seed population.  Ony works when Uproot and Death are FALSE.
#' @param P A list of parameters.
#' @param Data The Pre or Post output from an HBC simulation.
#' @keywords Plotting
#' @export
#
MigrationPlot <- function(P, Data, groups=NA, colorSet=NA){
  if(length(groups)==0){
    groups <- Getgroups(P, Data)
  }
  if(length(colorSet)==0){
    colorSet <- distinctColorPalette(length(groups))
  }
  par(mar=c(2,2,1,1), mgp=c(1.5,.5,0), bg="grey10", fg="white")
  plot(0, type="n", xlim=c(1, P$C), ylim=c(P$R,1),
       col.axis="white", font.axis=2)
    #test <- order(connect)
  #connect <- connect[test]
  #Terr <- Terr[test]
  #ColorList <- c("cadetblue1","cadetblue2","cadetblue3","cadetblue","cadetblue4",
  #               "slateblue1","slateblue2","slateblue3","slateblue","slateblue4",
  #               "darkorchid1","darkorchid2","darkorchid3","darkorchid","darkorchid4")
  #ColorVector <- rep(c(rep(ColorList,length(unique(connect))%/%length(ColorList)),
  #    ColorList[1:(length(unique(connect))%%length(ColorList))]),
  #    times=as.vector(table(connect)))
  for(i in seq_along(groups)){
    Modtest1 <- groups[[i]][,"Terr"]%%P$R
    Modtest2 <- groups[[i]][,"Connect"]%%P$R
    arrows(ceiling(groups[[i]][,"Terr"]/P$R), ifelse(Modtest1==0,P$R,Modtest1),
           ceiling(groups[[i]][,"Connect"]/P$R), ifelse(Modtest2==0,P$R,Modtest2),
           col=colorSet[[i]], angle=15, length=.1, code=1, lwd=2)
    #points(ceiling(Terr/P$R), ifelse((Terr)%%P$R==0,P$R,(Terr)%%P$R),
    #       cex=.6,pch=19, col="white")
    
  }  
}

#' Phoneme Frequency Plots
#'
#' Shows how common each phonemes is in the simulation color coded by population.
#' @param P A list of parameters.
#' @param Data The Pre or Post output from an HBC simulation.
#' @keywords Plotting
#' @export
#
PhonemeFrequencyPlots <- function(P, Data, groups=NA, colorSet=NA){
  if(length(groups)==0){
    groups <- Getgroups(P, Data)
  }
  if(length(colorSet)==0){
    colorSet <- randomColor(length(groups)) 
  }
  par(mar=c(3,3,1,1))
  plot(colSums(Data$Languages), type="l", col="White",col.axis="white",
       col.lab="white", xlab="Phonemes Ordered Most to Least Common",
       ylab=paste0("Number of Populations (Total=",nrow(Data$Languages),")"))
  for(i in seq_along(groups)){
    Sums <- colSums(Data$Languages[groups[[i]][,1],])
    plot(Sums, type="l",
         col=colorSet[i],col.axis="white",col.lab="white",
         xlab="Phonemes Ordered Most to Least Common",
         ylab=paste0("Number of Populations (Total=",length(groups[[i]][,1]),")"))
    points(which(Data$Languages[P$PopStart[i],]==1), rep(max(Sums),sum(Data$Languages[P$PopStart[i],])),
           col="White", cex=.6,pch=19)
    #PhoPerSeed[i,] <- Sums 
    print(summary(rowSums(Data$Languages[groups[[i]][,1],])))
    #Sums <- colSums(Data$Languages[groups[[i]][,1],])
    #segments(1:(728-1), Sums[1:(728-1)], 2:(728), Sums[2:(728)],col=colorSet[i])
  }  
}

#' Phoneme Population Frequency Plots
#'
#' Shows how common each phonemes is in the simulation color coded by population.
#' @param P A list of parameters.
#' @param Data The Pre or Post output from an HBC simulation.
#' @param groups Group structure of which territories were descended from what population seed.
#' @param colorSet The colors to use.
#' @param sort Whether to sort the data from most to least frequent phoneme.
#' @keywords Plotting
#' @export
#
PhonemePopulationFrequencyPlots <- function(P, Data, groups=NA, colorSet=NA, sort=TRUE){
  if(is.na(groups[1])){
    if(P$Death || P$UpRoot){
      groups <- GroupBySeed(P, Data)
    }else{
      groups <- GetGroups(P, Data)
    }
  }

  if(is.na(colorSet[1])){
    colorSet <- randomColor(length(groups)) 
  }
  print(colorSet)  
  if(sort){
    Data$Languages <- Data$Languages[,order(colSums(Data$Languages), decreasing = TRUE)]
  }
  print(Data$Languages[1:10, 1:10])
  
  PhoPerSeed <- matrix(0,nrow=length(groups),ncol=P$nPhon)
  
  for(i in seq_along(groups)){
    if(P$Death || P$UpRoot){
      Choices <- Data$Languages[groups[[i]],]
    }else{
      Data$Languages[groups[[i]][,1],]
    }
    PhoPerSeed[i,] <- colSums(Choices)
  }
  
  print(PhoPerSeed)
  plot(0,type = "n", xlim=c(0,P$nPhon), ylim=c(0,max(colSums(Data$Languages))),
       col.axis="white",col.lab="white", ylab="Number of populations",
       xlab="Phonemes, Ordered Common to Rare", font.lab=2, cex.lab=1, font.axis=2)
  
  for(i in 1:P$nPhon){
    rect(i-1,0,i,rev(cumsum(PhoPerSeed[,i])), col=rev(colorSet),border = NA)
  }
  
}

#' Get Colors
#'
#' Creates a color gradient.
#' @param P A list of parameters.
#' @param Data The Pre or Post output from an HBC simulation.
#' @keywords Plotting
#' @export
#
Getcolors <- function(P, Data, i, colors=c('coral1','coral4')){
  Order <- list()
  j <- 1
  Order[[1]] <- P$PopStart[i]
  while(length(Order[[j]])!=0){
    Order[[j+1]] <- which(Data$Populations$Founder %in% Data$Populations$ID[Order[[j]]])
    j <- j+1
  }
  
  Order[[j]] <- NULL
  Order[[1]] <- NULL
  Pal <- colorRampPalette(colors)
  names(Order) <- Pal(length(Order))
  Order <- setNames(unlist(Order, use.names=F),rep(names(Order), lengths(Order)))
  return(names(sort(Order)))
  
}


#Gets the distance between two points
#' Phoneme Mantel
#'
#' Performs both a Hamming and Jaccard Mantel test.
#' @param P A list of parameters.
#' @param Data The Pre or Post output from an HBC simulation.
#' @param repeats How many times to repeat the analysis.
#' @keywords Plotting
#' @export
#
PhonemeMantel <- function(P, Data, repeats=100){
  time <- proc.time()
  #get distances; Geo, Jac, and Ham
  DistMat <- MakeDistanceMap(P)
  Jac <- distance(Data$Languages, method="jaccard")
  Ham <- distance(Data$Languages, method="manhattan")
  par(mfrow=c(1,2))
  image(Jac)
  image(Ham)
  #get into right form
  FinalGeo <- as.dist(DistMat)
  FinalJac <- as.dist(Jac)
  FinalHam <- as.dist(Ham)
  
  #perform tests
  print(mantel.rtest(FinalGeo, FinalJac, repeats))
  print(mantel.rtest(FinalGeo, FinalHam, repeats))
  proc.time()-time
}

#' Make Distance Map
#'
#' Creates a distance map based on the euclidian diatcnes between territories.
#' @param P A list of parameters.
#' @keywords Plotting
#' @export
#
MakeDistanceMap <- function(P){
  DistMat <- matrix(0, nrow=2000,ncol=2000)
  for(i in 1:2000){
    for(j in 1:2000){
      DistMat[i,j]<-GetDist(P,i,j)    
    }
  }
  return(DistMat)
}

#' Get Distance
#'
#' Get the euclidian distance between two territories.
#' @param P A list of parameters.
#' @param point1 One territory location.
#' @param point2 Another territory location.
#' @keywords Plotting
#' @export
#
GetDist <- function(P, point1, point2){
  XY <- GetXYCoords(P, rbind(point1, point2))
  A <- XY[1,1]-XY[2,1]
  B <- XY[1,2]-XY[2,2]
  return(sqrt(A^2+B^2))
}


#' Get XY Coordinates
#'
#' COnverts territory numbers into X,Y corrdinates.
#' @param P A list of parameters.
#' @param territories A vector of territory indicies to convert into X,Y coordinates.
#' @keywords Plotting
#' @export
#
GetXYCoords <- function(P, territories){
  Xs <- integer(length(territories))
  Ys <- integer(length(territories))
  for(i in 1:length(territories)){
    Ys[i] <- territories[i]%%P$R
    if(Ys[i] == 0){
      Xs[i] <- territories[i]%/%P$R
    }else{
      Xs[i] <- territories[i]%/%P$R+1
    }

    if(Ys[i]==0){
      Ys[i] <- P$R
    }
  }
  return(cbind(Xs,Ys))
}


#' Get Bering Strait Coordinates
#'
#' Returns the hardcoded locations of the Bering Strait boundaries.
#' @keywords Plotting
#' @export
#
GetBeringCoords <- function(){
  AsiaLowerRight <- P$R*20
  AsiaUpperRight <- AsiaLowerRight-15
  AsiaBearingCorner <- P$R*4-15
  BerringNAmericaCorner <- P$R*3 + 5
  NAmericanlowerRight <- AsiaUpperRight + P$R*18
  NAmericanLowerEntry <- NAmericanlowerRight - 5
  NAmericanUpperRight <- NAmericanLowerEntry%/%P$R*P$R+1
  
  #vertical boundaries are imposed rightward/increasing X index
  Vert<-GetXYCoords(P, c(AsiaLowerRight:AsiaUpperRight,
                         AsiaBearingCorner:BerringNAmericaCorner,
                         NAmericanLowerEntry:NAmericanUpperRight))
  Vert[,1] <- Vert[,1]+.5
  #horizontal boundaries are imposed downward/increasing Y index
  Horz <- GetXYCoords(P, c(seq(AsiaUpperRight,AsiaBearingCorner, by=-P$R)-1,
                           seq(AsiaUpperRight, NAmericanlowerRight, by=P$R)-1))
  Horz[,2] <- Horz[,2]+.5
  return(rbind(Vert,Horz))
}

#' Save Data
#'
#' Saves just the language data from the simulation to .csv files.
#' @keywords Plotting
#' @export
#
SaveData <- function(Data, filename){
  write.csv(Data$NoHorizontal$Languages, paste0(filename, "-pre.csv"))
  write.csv(Data$Horizontal$Languages, paste0(filename, "-post.csv"))
}

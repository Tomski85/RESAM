################################################################################
#                     Cost Path with land use instrument                       #
################################################################################

## Creating the function
# Automated calculation of the least cost path for complete raster file using 
#centroids as input and various points
#Defining the transition matrix (cost raster)
#Defining the End Points of the least cost path calculation (conPoints)
#Defining the Start Points (cellPoints)

leastCostPathRaster <- function(tranMatrix,cellPoints,conPoints){
data <- data.frame() 
for (i in 1:nrow(cellPoints)){
  cd  <- round(as.data.frame((t(costDistance(tranMatrix,cellPoints[i,],conPoints)))), digits = 2)
  cd$pts_sel <-as.numeric(row.names(cd))
  a   <- min(cd$V1)
  cd  <- subset(cd,cd$V1==a)
  cd$pts_pol <- i
  cd <- cd[-(2:10),]
  data <- rbind(data,cd)
}  
return(data)    
}

################################################################################
#          Arising External Cost per Cost Path (Instrument-Scenario)           #
################################################################################

## Creating the function
# Calculation of the arising cost for a defined route 

pathCostCalulation <- function(dataInput,tranMatrix,cellPoints,conPoints,costRaster){
  for (i in 1:nrow(dataInput)){
    # which vertices are optimal for the raster cell
    a   <- dataInput[dataInput$ID3==i,c("pts_sel")]
    aa  <- dataInput[dataInput$ID3==i,c("ID")]
    # Transformation of the shortest path to a polygone route
    line_cn          <- shortestPath(tranMatrix, cellPoints[aa,], conPoints[a,], output="SpatialLines")
    line_cn          <- spsample(line_cn,n=100,type="regular")
    #Extraction of the external spatial cost
    extract          <-(extract(costRaster,line_cn,line_cn=T,cellnumber=T,along=T,df=T))
    extract          <- unique(extract[c(2,3)])
    names (extract) [c(1:2)]  <- c("cell","layer")
    extract$cell2    <- append(extract$cell,NA)[c(-1)]
    extract$layer2   <- append(extract$layer,NA)[c(-1)]
    extract$weight   <- ifelse(extract$cell+49 == extract$cell2 | extract$cell-49 == extract$cell2 | extract$cell+1 == extract$cell2 | extract$cell-1                        == extract$cell2,1,round(sqrt(2),digit=3))
    extract$value    <- ifelse(is.na(extract$weight),0,(extract$layer + extract$layer2) / 2 * extract$weight) 
    #Sum of the cost has to be improved
    d                <-  as.numeric(colSums(extract[c(6)]))
    dataInput[dataInput$ID==aa,c("V2")] <- d
  }
  return(dataInput$V2)
}
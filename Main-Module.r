################################################################################               
#                                                                              #
#                    Model-Preparation (Setup Parameters)                      #
#                                                                              #
################################################################################

ID_L <- 0
dat <- data.frame()
nodat <- data.frame()


################################################################################               
#                 Setting all parameters and import to NetLogo                 #
################################################################################

ID_L <- 0
lu_res <- 0
repeat{ 

ID_L <- ID_L + 1

for (iii in 1:nrow(sec))
{
  #Setting the market scenario 
  if(sec[sec$ID==iii,c("MS2")] == 0)
  {NLCommand("set MS2 false", nl.obj=nl.test1)}
  else
  {NLCommand("set MS2 true", nl.obj=nl.test1)}   
  
  # MS3
  if(sec[sec$ID==iii,c("MS3")] == 0)
  {NLCommand("set MS3 false", nl.obj=nl.test1)}
  else
  {NLCommand("set MS3 true", nl.obj=nl.test1)}  
  
  # MS3.1
  if(sec[sec$ID==iii,c("MS3.1")] == 0)
  {NLCommand("set MS3.1 false", nl.obj=nl.test1)}
  else
  {NLCommand("set MS3.1 true", nl.obj=nl.test1)}  
  
  # CO
  if(sec[sec$ID==iii,c("CO")] == 1)
  {NLCommand("set CO true", nl.obj=nl.test1)}
  else
  {NLCommand("set CO false", nl.obj=nl.test1)}  
  
  # EX
  if(sec[sec$ID==iii,c("EX")] == 1)
  {NLCommand("set EX true", nl.obj=nl.test1)}
  else
  {NLCommand("set EX false", nl.obj=nl.test1)} 
  
  # SU
  if(sec[sec$ID==iii,c("SU")] == 1)
  {NLCommand("set SU true", nl.obj=nl.test1)}
  else
  {NLCommand("set SU false", nl.obj=nl.test1)} 
  
  # PA
  NLCommand("set PA",sec[sec$ID==iii,c("PA")], nl.obj=nl.test1)
  
  #Setup model
  if(sec[sec$ID==iii,c("ID_S")] == 1) 
  {
    
    #Creating Consumer Centre Demand  
    Y_D_0 <- round(runif(1,sc[sc$ID==1,c("Y_D_0")][[1]][1],sc[sc$ID==1,c("Y_D_0")][[1]][2]),digits=2)
    Y_D_1 <- round(runif(1,sc[sc$ID==1,c("Y_D_1")][[1]][1],sc[sc$ID==1,c("Y_D_1")][[1]][2]),digits=2)
    Y_D_2 <- round(runif(1,sc[sc$ID==1,c("Y_D_2")][[1]][1],sc[sc$ID==1,c("Y_D_2")][[1]][2]),digits=2)
    Y_D       <-  Y_D_0 + Y_D_1 + Y_D_2
    #Creating Landscape parameters
    x_s_max <- round(runif(1,sc[sc$ID==1,c("x_s_max")][[1]][1],sc[sc$ID==1,c("x_s_max")][[1]][2]),digits=4)#sec[sec$ID==iii,c("x_s_max")]
    h_max   <- round(runif(1,sc[sc$ID==1,c("h_max")][[1]][1],sc[sc$ID==1,c("h_max")][[1]][2]),digits=4)#sec[sec$ID==iii,c("h_max")]
    alfa    <- round(runif(1,sc[sc$ID==1,c("alfa")][[1]][1],sc[sc$ID==1,c("alfa")][[1]][2]),digits=4)#sec[sec$ID==iii,c("alfa")]
    beta    <- round(runif(1,sc[sc$ID==1,c("beta")][[1]][1],sc[sc$ID==1,c("beta")][[1]][2]),digits=4)#sec[sec$ID==iii,c("beta")] 
    
    # Creating Model Parameters
    w_PC     <-round(runif(1,sc[sc$ID==1,c("w_PC")][[1]][1],sc[sc$ID==1,c("w_PC")][[1]][2]),digits=4)
    w_GC     <-w_PC * 0.02
    delta    <-round(runif(1,sc[sc$ID==1,c("delta")][[1]][1],sc[sc$ID==1,c("delta")][[1]][2]),digits=4)
    l        <-round(runif(1,sc[sc$ID==1,c("l")][[1]][1],sc[sc$ID==1,c("l")][[1]][2]),digits=4)
    m        <-round(runif(1,sc[sc$ID==1,c("m")][[1]][1],sc[sc$ID==1,c("m")][[1]][2]),digits=4)
    p        <-sc[sc$ID==1,c("p")]
    p_max    <-round(runif(1,sc[sc$ID==1,c("p_max")][[1]][1],sc[sc$ID==1,c("p_max")][[1]][2]),digits=4)
    w_GE     <-round(runif(1,sc[sc$ID==1,c("w_GE")][[1]][1],sc[sc$ID==1,c("w_GE")][[1]][2]),digits=4)
    theta    <-round(runif(1,sc[sc$ID==1,c("theta")][[1]][1],sc[sc$ID==1,c("theta")][[1]][2]),digits=4) 
    
    #Policy parameter
    grid_ratio  <-round(runif(1,0,1),digits=4)
    psi         <--0.2 #round(runif(1,-0.5,0),digits=4)
    
    #Coalition Parameter
    lambda      <-round(runif(1,0.1),digits=4)
    gamma       <-round(runif(1,0.1),digits=4)
    s           <-round(runif(1,0.0,1),digits=4)
    t           <-s #round(runif(1,0.0,0.175),digits=4)
    
    #Export cost parameters
    NLCommand("set l",l, nl.obj=nl.test1)
    NLCommand("set p",sc[sc$ID==1,c("p")], nl.obj=nl.test1)
    NLCommand("set p_max",p_max, nl.obj=nl.test1)
    NLCommand("set m",m, nl.obj=nl.test1)
    NLCommand("set w_PC",w_PC, nl.obj=nl.test1)
    NLCommand("set w_GC",w_GC, nl.obj=nl.test1)
    NLCommand("set w_GE",w_GE, nl.obj=nl.test1)
    NLCommand("set delta",delta, nl.obj=nl.test1)
    NLCommand("set theta",theta, nl.obj=nl.test1)
    NLCommand("set grid_ratio",grid_ratio, nl.obj=nl.test1)
    NLCommand("set psi",psi, nl.obj=nl.test1)
    NLCommand("set lambda",lambda, nl.obj=nl.test1)
    NLCommand("set gamma",gamma, nl.obj=nl.test1)
    NLCommand("set s",s, nl.obj=nl.test1)
    NLCommand("set t",s, nl.obj=nl.test1)
    
    #Export of landscape variables 
    NLCommand("set Y_D_0",Y_D_0, nl.obj=nl.test1)
    NLCommand("set Y_D_1",Y_D_1, nl.obj=nl.test1)
    NLCommand("set Y_D_2",Y_D_2, nl.obj=nl.test1)
    NLCommand("set x_s_max",x_s_max, nl.obj=nl.test1)
    NLCommand("set h_max",h_max, nl.obj=nl.test1)
    NLCommand("set alfa",alfa, nl.obj=nl.test1)
    NLCommand("set beta",beta, nl.obj=nl.test1)

################################################################################               
#                   Starting the NetLogo application                           #
################################################################################    
    
  NLCommand("setup", nl.obj=nl.test1)
  NLCommand("setup-turtles", nl.obj=nl.test1)
  }
  else
  {
    NLCommand("setup-turtles", nl.obj=nl.test1)
  }

################################################################################
#                                                                              #
#                            Spatial Module                                    #
#                                                                              #
################################################################################

################################################################################
#                       Import from NetLogo to R                               #
################################################################################

#get patches
patchesSt <-NLGetPatches(c("pxcor","pycor","x_d_0","x_d_1","x_d_2","x_d","x_p","y","h","city_id","x_s","d_all"),"patches",nl.obj=nl.test1)
for (rr in 3:12) {
patchesSt[,rr] <- ifelse(patchesSt$pycor == 11  | patchesSt$pycor == -11 |patchesSt$pxcor == 15 | patchesSt$pxcor == -15 ,NA,patchesSt[,rr])
}
patchesNL <- patchesSt
patches   <- patchesNL
patches$ID <- as.character(seq(nrow(patches)))
#convert to SpatialPointDataFrame
coordinates(patches)  <- ~pxcor+pycor
#convert to SpatialPixelDataFrame
gridded(patches) <- TRUE

#get cities
citiesNL <-NLGetAgentSet(c("who","xcor","ycor","y_d_j"),"cities",nl.obj=nl.test1)
Y_d_j  <-  as.numeric(colSums(citiesNL[c("y_d_j")]))
cities   <- citiesNL
#convert to SpatialPointDataFrame
coordinates(cities)  <- ~xcor+ycor
#convert to SpatialPixelDataFrame
gridded(cities) <- TRUE

## safety plot
#spplot(patches, 'c_l', col.regions=colorRampPalette(c("white","darkgreen")), xlab="x",ylab="y",
#sp.layout = list("sp.points",cities, col="black",lwd=2))

################################################################################
#                    Application of Regulation Policies                        #
################################################################################


##cost path preparation
m_x_s            <- round(t(as.matrix(patches[9])),digits = 2)
m_x_d            <- round(t(as.matrix(patches[10])),digits = 2)
m_x_h            <- round(t(as.matrix(patches[7])),digits = 2)
m_x_x            <- round(t(as.matrix(patches[5])),digits = 2)
m_x_city         <- round(t(as.matrix(patches[8])),digits = 2)

m_1              <- ifelse(is.na(m_x_s),1,1)
#only site-dependent costs 
m_x_p            <- round(t(as.matrix(patches[9])),digits = 2)

## Site and Distance-dependent land-use restrictions 

#distance restrictions
if (sec[sec$ID==iii,c("lui_d")] > 0)
{lui_d_sec  <- round(runif(1,0,95),digits=3)} else {lui_d_sec <- 0}

lu_d                         <- max(na.omit(patchesSt$d_all)) + 0.5

repeat
{
  lu_d                         <- lu_d - 0.05
  m_x_lui_d                    <- ifelse(m_x_d > lu_d,NA,1)
  lu_res <-(sum(is.na(m_x_lui_d)) - sum(is.na(m_x_p)) + sum(is.na(m_x_s)) - sum(is.na(m_x_p))) / sum(!is.na(m_x_p)) * 100 
  if (lu_res < (lui_d_sec + 10) & lu_res > (lui_d_sec - 1.5)) {break}   
}

m_x_lui_d                    <- ifelse(m_x_d > lu_d,NA,1)

#Site restrictions
lui_s_sec                    <- round(runif(1,0,100),digits=2) 
if (sec[sec$ID==iii,c("lui_s")] > 0) 
{lui_s_sec  <- round(runif(1,0,95),digits=3)} else {lui_s_sec <- 0}

lu_s                         <- max(na.omit(patchesSt$x_s)) + 0.5

repeat
{
  lu_s                         <- lu_s - 0.01
  m_x_lui_s                    <- ifelse(m_x_s > lu_s,NA,1)
  lu_res <-(sum(is.na(m_x_d)) - sum(is.na(m_x_p)) + sum(is.na(m_x_lui_s)) - sum(is.na(m_x_p))) / sum(!is.na(m_x_p)) * 100 
  if (lu_res < (lui_s_sec  + 10) & lu_res > (lui_s_sec - 1)) {break}  
}

m_x_lui_s                    <- ifelse(m_x_s > lu_s,NA,1)
m_x_lui_s_ex                 <- m_x_lui_s
lu_s_ex                      <- lu_s

#cells with restrictions
lu_res    <- (sum(is.na(m_x_lui_d)) - sum(is.na(m_x_p)) + sum(is.na(m_x_lui_s)) - sum(is.na(m_x_p))) / sum(!is.na(m_x_p)) 

#exceptions for site-related restriction by high rates
if (lu_res > 0.50 & lui_s_sec > 50)
{lu_s                         <- max(na.omit(patchesSt$x_s)) + 0.5
 repeat
 {
   lu_s                         <- lu_s - 0.01
   m_x_lui_s                    <- ifelse(m_x_s > lu_s,NA,1)
   lu_res_tmp <-(sum(is.na(m_x_d)) - sum(is.na(m_x_p)) + sum(is.na(m_x_lui_s)) - sum(is.na(m_x_p))) / sum(!is.na(m_x_p)) * 100 
   if (lu_res_tmp < (50 + 5) & lu_res_tmp > (50 - 1)) {break}  
 } 
 m_x_lui_s                    <- ifelse(m_x_s > lu_s,NA,m_x_s)
 m_x_lui_s                    <- ifelse(m_x_lui_s > lu_s_ex,2,1)  
}

#catchment areas
m_city_id           <- t(as.matrix(patches[8]))

## Area designation for the regions by wind conditions and externalities 
m_x_0 <-  ifelse(m_x_city == 0,1,0)
m_x_1 <-  ifelse(m_x_city == 1,1,0)
m_x_2 <-  ifelse(m_x_city == 2,1,0)

# Area desgination max wind conditions
if (sec[sec$ID==iii,c("lui_h")] < 100)
{lui_h_sec  <- round(runif(1,0,95),digits=3)} else {lui_h_sec <- 100}  

lu_h0                         <- max(subset(patchesSt,city_id == 0 ,h) + 0.5)
lu_h1                         <- max(subset(patchesSt,city_id == 1 ,h) + 0.5)
lu_h2                         <- max(subset(patchesSt,city_id == 2 ,h) + 0.5)

repeat
{
  lu_h0                         <- lu_h0 - 0.01
  m_x_lui_h0                     <- m_x_h * m_x_0
  m_x_lui_h0                     <- ifelse(m_x_lui_h0 > lu_h0,999,m_x_lui_h0)
  lu_res <- length(which(m_x_lui_h0 == 999)) / length(which(m_x_0 == 1)) * 100 
  if (lu_res < (lui_h_sec + 10) & lu_res >= (lui_h_sec)) {break}   
}

repeat
{
  lu_h1                         <- lu_h1 - 0.01
  m_x_lui_h1                     <- m_x_h * m_x_1
  m_x_lui_h1                     <- ifelse(m_x_lui_h1 > lu_h1,999,m_x_lui_h1)
  lu_res <- length(which(m_x_lui_h1 == 999)) / length(which(m_x_1 == 1)) * 100 
  if (lu_res < (lui_h_sec + 10) & lu_res >= (lui_h_sec)) {break}   
}

repeat
{
  lu_h2                         <- lu_h2 - 0.01
  m_x_lui_h2                     <- m_x_h * m_x_2
  m_x_lui_h2                     <- ifelse(m_x_lui_h2 > lu_h2,999,1)
  lu_res <- length(which(m_x_lui_h2 == 999)) / length(which(m_x_2 == 1)) * 100 
  if (lu_res < (lui_h_sec + 10) & lu_res >= (lui_h_sec)) {break}   
}

m_x_lui_h <- m_x_lui_h0 * m_x_lui_h1 * m_x_lui_h2
lu_res <- length(which(m_x_lui_h == 999)) / sum(!is.na(m_x_p)) * 100

m_x_lui_h                    <- ifelse(m_x_lui_h == 1,NA,1)

# Area desgination min externalities 
if (sec[sec$ID==iii,c("lui_ex")] < 100)
{lui_x_sec  <- 50} else {lui_x_sec <- 100}   #round(runif(1,0,100),digits=3)

lu_x0                         <- -1
lu_x1                         <- -1
lu_x2                         <- -1

repeat
{
  lu_x0                          <- lu_x0 + 0.01
  m_x_lui_x0                     <- m_x_x * m_x_0
  m_x_lui_x0                     <- ifelse(m_x_lui_x0 < lu_x0 & m_x_lui_x0 > 0,999,1)
  lu_res <- length(which(m_x_lui_x0 == 999)) / length(which(m_x_0 == 1)) * 100 
  if (lu_res < (lui_x_sec + 10) & lu_res >= (lui_x_sec)) {break}   
}

repeat
{
  lu_x1                          <- lu_x1 + 0.01
  m_x_lui_x1                     <- m_x_x * m_x_1
  m_x_lui_x1                     <- ifelse(m_x_lui_x1 < lu_x1 & m_x_lui_x1 > 0,999,1)
  lu_res <- length(which(m_x_lui_x1 == 999)) / length(which(m_x_1 == 1)) * 100 
  if (lu_res < (lui_x_sec + 10) & lu_res >= (lui_x_sec)) {break}   
}

repeat
{
  lu_x2                          <- lu_x2 + 0.01
  m_x_lui_x2                     <- m_x_x * m_x_2
  m_x_lui_x2                     <- ifelse(m_x_lui_x2 < lu_x2 & m_x_lui_x2 > 0,999,1)
  lu_res <- length(which(m_x_lui_x2 == 999)) / length(which(m_x_2 == 1)) * 100 
  if (lu_res < (lui_x_sec + 10) & lu_res >= (lui_x_sec)) {break}   
}

m_x_lui_x <- m_x_lui_x0 * m_x_lui_x1 * m_x_lui_x2
lu_res <- length(which(m_x_lui_x == 999)) / sum(!is.na(m_x_p)) * 100

m_x_lui_x                    <- ifelse(m_x_lui_x == 1,NA,1)

################################################################################
#             Calculation Preparation of Connection Grid Costs                 #
################################################################################

# matrix to raster
r_x_lui            <- raster(m_x_lui_s,xmn=-15,xmx=15,ymn=-11,ymx=11,crs="+proj=utm +units=m")
r_x_p              <- raster(m_x_p,xmn=-15,xmx=15,ymn=-11,ymx=11,crs="+proj=utm +units=m")
r_1                <- raster(m_1,xmn=-15,xmx=15,ymn=-11,ymx=11,crs="+proj=utm +units=m")     

#plot(r_x_lui)
#text(r_x_lui)

#raster to polygone
pol_r_x_p  <- rasterToPolygons(r_x_p)
pol_r_x_p@data$ID <-as.character(seq(nrow(pol_r_x_p)))

# centroids of polygones
pts_pol_x_p    <- coordinates(pol_r_x_p)

## safety plot Checking ID큦
#plot(SpatialPoints(pts_pol_x_p),pch=20, col="red")
#text(pts_pol_x_p[,1]+1, pts_pol_x_p[,2]+1, 1:nrow(pts_pol_x_p))

#Create TransitionMatrix with 8 directions
tr_1         <- transition(1/r_1,transitionFunction=mean, directions=8)
tr_x_lui     <- transition(1/r_x_lui,transitionFunction=mean, directions=8)
tr_x_p       <- transition(1/r_x_p,transitionFunction=mean, directions=8)

#Correction of the matrix because of diagonal movement
trc_1       <- geoCorrection(tr_1,type="c",multpl=F, scl=T)
trc_x_lui   <- geoCorrection(tr_x_lui,type="c",multpl=F, scl=T)
trc_x_p     <- geoCorrection(tr_x_p,type="c",multpl=F, scl=T)
## create citie points
citie_p      <- as.matrix(citiesNL[c(2,3)])

###################Distribution Network: Line to Points#######################                        #

#generate the lines
l_1_1  <- shortestPath(trc_1, citie_p[1,], citie_p[2,], output="SpatialLines")
l_1_2  <- shortestPath(trc_1, citie_p[2,], citie_p[3,], output="SpatialLines")
l_1_3  <- shortestPath(trc_1, citie_p[1,], citie_p[3,], output="SpatialLines")

#raster to polygone
pol_r_1  <- rasterToPolygons(r_1)
pol_r_1@data$ID <-as.character(seq(nrow(patchesSt)))

#Line to regular points
pts_1_1   <- spsample(l_1_1,n=100,type="regular")
pts_1_2   <- spsample(l_1_2,n=100,type="regular")
pts_1_3   <- spsample(l_1_3,n=100,type="regular")

#mark all polygones intersect by points
pol_sel_r_1_1 <- over(pts_1_1,pol_r_1)
pol_sel_r_1_2 <- over(pts_1_2,pol_r_1)
pol_sel_r_1_3 <- over(pts_1_3,pol_r_1)

#merge and unique the point.data.frame
pol_sel_r_1   <-rbind(pol_sel_r_1_1,pol_sel_r_1_2,pol_sel_r_1_3)
pol_sel_r_1   <-unique(pol_sel_r_1)

#select polygones
pol_sel_1 <- subset(pol_r_1,pol_r_1@data$ID %in% pol_sel_r_1$ID)
#plot(pol_sel_1)

# centroids of polygones
pts_sel_1     <- coordinates(pol_sel_1)
row.names(pts_sel_1) <- seq_len(nrow(pts_sel_1))

## safety plot
#plot(r_1)
#plot(SpatialPoints(pts_sel_1),add=T,pch=20, col="red")

################################################################################
#         Cost Path with land use instrument (Instrument-Scenario)             #
################################################################################

#raster to polygone
pol_r_x_lui  <- rasterToPolygons(r_x_lui)
pol_r_x_lui@data$ID <-as.character(seq(nrow(pol_r_x_lui)))

# centroids of polygones
pts_pol_x_lui    <- coordinates(pol_r_x_lui)

#Selcetion of existing connection points
pts_sel_x_lui    <- SpatialPoints(pts_sel_1, proj4string = CRS("+proj=utm +units=m"))
pts_sel_x_lui    <- over(pts_sel_x_lui,pol_r_x_lui)
pts_sel_x_lui    <- subset(pol_r_x_lui,pol_r_x_lui@data$ID %in% pts_sel_x_lui$ID)
pts_sel_x_lui    <- coordinates(pts_sel_x_lui)
row.names(pts_sel_x_lui) <- seq_len(nrow(pts_sel_x_lui))

#plot(pol_r_x_lui)
#plot(SpatialPoints(pts_sel_x_lui),add=T,pch=20, col="red")

#Connection between r_1 and r_x_lui
pts_pol_1 <- coordinates(pol_r_1)
cd_ID     <- SpatialPoints(pts_pol_1, proj4string = CRS("+proj=utm +units=m"))
cd_ID     <- over(cd_ID,pol_r_x_lui)
cd_ID$ID2    <- seq(nrow(cd_ID))

## safety plot example poylgone 1
#plot(r_x_lui)
#plot(pol_r_x,add=T)
##plot(SpatialPoints(pts_sel_x_lui),add=T,pch=20, col="red")
#plot(SpatialPoints(citie_p), add=TRUE, pch=20, col="black", lwd=3)
#plot(shortestPath(trc_x_lui, pts_pol_x_lui[972,], pts_sel_x_lui[5,], output="SpatialLines"),add=T)

#minimal costpath for all raster_cells via a function
cd_min_x_lui_ <- leastCostPathRaster(trc_x_lui,pts_pol_x_lui,pts_sel_x_lui)

#create raster out of data.frame.result                           
cd_min_x_lui_<- cd_min_x_lui_[cd_min_x_lui_$V1 != "Inf",c("V1","pts_sel","pts_pol")]
cd_min_x_lui <- merge(cd_ID,cd_min_x_lui_,by.x="ID",by.y="pts_pol",all.x=T) 
cd_min_x_lui <- cd_min_x_lui[order(cd_min_x_lui$ID2),] 


################################################################################
#                  Arising External Cost per Cost Path                         #
################################################################################

# seperation of cases with 0 costs                                                         
cd_min_x     <- cd_min_x_lui[!is.na(cd_min_x_lui$V1) & cd_min_x_lui$V1 > 0,c(1,3,4,5)]
cd_min_x$ID3 <- seq_len(nrow(cd_min_x))

# loop for kumulative route calculation of access network via a function
V2 <- pathCostCalulation(cd_min_x,trc_x_lui,pts_pol_x_lui,pts_sel_x_lui,r_x_p)
cd_min_x$V2 <- V2

# reprocessing of the table # merge with result with table form the first loop
# replacement of NA큦 with 0
cd_min <- merge(cd_min_x_lui,cd_min_x[,c("ID2","V2")],by="ID2",all.x=T)
cd_min$V2 <- ifelse(is.na(cd_min$V2) & cd_min$V1 == 0,0,cd_min$V2)
cd_min <- cd_min[order(cd_min_x_lui$ID2),]  

#resulting external cost
m_cd_x_T_IS                                    <- round(t(matrix(cd_min$V2,nrow=31,ncol=23)),digits = 2)

## safety plot  
#plot(r_cd_x_T_IS)
#plot(SpatialPoints(pts_sel_x_lui),add=T,pch=20, col="red")

# resulting connection grid cost
m_cd_d_iv_IS                                   <- round(t(matrix(cd_min$V1,nrow=31,ncol=23)),digits = 2)
m_cd_d_iv_IS                                   <- ifelse(m_cd_d_iv_IS>500,35,m_cd_d_iv_IS)

# including restrictions of social related land use instruments
m_cd_d_iv_IS    <- m_cd_d_iv_IS *  m_x_lui_d * m_x_lui_s_ex * m_x_lui_h * m_x_lui_x

lu_res          <- sum(!is.na(m_cd_d_iv_IS)) / sum(!is.na(m_x_p)) * 100

m_cd_d_iv_IS    <- ifelse(is.na(m_cd_d_iv_IS),888,m_cd_d_iv_IS)
m_cd_d_iv_IS[c(1,23),]          <- NA 
m_cd_d_iv_IS[,c(1,31)]          <- NA 

################################################################################
#                      Export from R to NetLogo                                #
################################################################################


#prohibition of plant construction on grid cells
m_cd_d_iv_IS    <- ifelse(m_cd_d_iv_IS == 0,999,m_cd_d_iv_IS)
m_cd_d_iv_IS    <- ifelse(is.na(m_cd_d_iv_IS),999,m_cd_d_iv_IS)
NLSetPatches("d_iv_IS",m_cd_d_iv_IS, nl.obj=nl.test1)

#cost path with external cost Instrument-Scenario
m_cd_x_T_IS       <- ifelse(is.na(m_cd_x_T_IS),999,m_cd_x_T_IS)
NLSetPatches("x_T_IS",m_cd_x_T_IS, nl.obj=nl.test1)
  
################################################################################
#                     Export from NetLogo to R                                 #
################################################################################

## Create Database Connection table between [results GIS-M,Patch-Information,Grid-Points PTDF, Grid-Points GIS-M]
#Setup-Turtels
NLDoCommand(1,"setup-turtles",nl.obj=nl.test1)
#Patch and results GIS-M
patchesNL    <-NLGetPatches(c("pxcor","pycor","phi_IS","x_IS","y","city_id","d_iv_IS","x_p"),"patches",nl.obj=nl.test1)

#Restore storage
patchesNL$ID <-as.numeric(row.names(patchesNL))
patchesNL    <-merge(patchesNL,cd_min_x_lui[c("ID2","pts_sel")],by.x="ID",by.y="ID2")
names(patchesNL)[c(10)]  <- c("pts_sel_IS")     # After adding new vaiables review necessary

#Grid-Points GIS-M and Grid-Points PTDF
pts_sel_x_df    <- round(data.frame(pts_sel_x_lui),5)
pts_sel_x_df$ID <-row.names(pts_sel_x_df)          
pts_net_1_df    <- round(pts_net[c("ID","GeoX","GeoY")],5)
pts             <- merge(pts_sel_x_df,pts_net_1_df,by.x="X1",by.y="GeoX",all.y=T)
pts             <- pts[pts$X2 == pts$GeoY & !is.na(pts$ID.x),c(3,4)]

#Patch and Grid-Points
patchesNL       <- merge(patchesNL,pts[c("ID.x","ID.y")],by.x="pts_sel_IS",by.y="ID.x",all.x=T)
patchesNL       <- patchesNL[order(patchesNL$ID),]

################################################################################
#                Setup of the variables in NetLogo                             #
################################################################################

for (ii in 1:nrow(sc)){
  
################################################################################
#                                                                              #
#                             DC-Model                                         #
#                                                                              #
################################################################################

################################################################################
#           Calculation of line capacity for the baseline scenario             #
################################################################################

#Definition of the generation points
gen       <-  as.vector(sc[sc$ID==ii,c("gen")])[[1]]
gen_y_d   <-  as.vector(c(list(c(Y_D_0,Y_D_1,Y_D_2))))[[1]]

##Selection of the relevant power transfer distribution factors
ptdf_sel  <-  ptdf[c(paste(0,gen,sep="_"),paste(1,gen,sep="_"),paste(2,gen,sep="_"))]
 
##Calculation of the capacity per city and per generation plant
line_c0  <- t((gen_y_d * (Y_D_0 /(Y_D_0 + Y_D_1 + Y_D_2))) * t(ptdf_sel[,(paste(0,gen,sep="_"))]))
line_c1  <- t((gen_y_d * (Y_D_1 /(Y_D_0 + Y_D_1 + Y_D_2))) * t(ptdf_sel[,(paste(1,gen,sep="_"))]))
line_c2  <- t((gen_y_d * (Y_D_2 /(Y_D_0 + Y_D_1 + Y_D_2))) * t(ptdf_sel[,(paste(2,gen,sep="_"))]))

line_cap_t0  <-  as.data.frame(rowSums(cbind(line_c0,line_c1,line_c2))) * 5 


################################################################################
#                      Nodal Pricing Model                                     #
################################################################################

##Calculate DC-Model estimate line sequments by generation
      
      #Calculate generation form transformer station at t=0
Y_s      <- 0     
gen_t    <- Y_D - Y_s
      #Create vector with generation cap and ID and select relevant PTDF큦
ref_np    <- data.frame(1:nrow(pts_net))
names (ref_np) [c(1)]  <- c("ID")

for (i in 1:nrow(pts_net))
{
cp        <-  (append(as.vector(i),gen))
      #Reduction of conventional capcity (rethink)

v_y_d     <-  append(as.vector(h_max),gen_y_d-h_max/length(gen_y_d))
df_y_d    <-  as.data.frame(cbind(cp,v_y_d))
df_y_d    <-  aggregate(df_y_d,list(df_y_d$cp),sum)

ptdf_sel  <-  ptdf[c(paste(0,cp,sep="_"),paste(1,cp,sep="_"),paste(2,cp,sep="_"))]
      
      #Calculate line_cap of all transactions
line_c0  <- t((v_y_d * (Y_D_0 /(Y_D_0 + Y_D_1 + Y_D_2))) * t(ptdf_sel[,(paste(0,cp,sep="_"))]))
line_c1  <- t((v_y_d * (Y_D_1 /(Y_D_0 + Y_D_1 + Y_D_2))) * t(ptdf_sel[,(paste(1,cp,sep="_"))]))
line_c2  <- t((v_y_d * (Y_D_2 /(Y_D_0 + Y_D_1 + Y_D_2))) * t(ptdf_sel[,(paste(2,cp,sep="_"))]))
line_cap  <-  as.data.frame(rowSums(cbind(line_c0,line_c1,line_c2)))
     
      #Caluclation of the line sequments which have to be reinforced if x>0
reinforce    <-  abs(line_cap) - abs(line_cap_t0) 
reinforce$ID <-  as.numeric(row.names(reinforce))
reinforce    <-  merge(reinforce,pts_net[c("ID","col1_r","col2_r","loads")],by="ID")
reinforce    <-  merge(reinforce,pts_net[c("ID","col1_r","col2_r","loads")],by="ID")
reinforce    <-  merge(reinforce,df_y_d[c("Group.1","v_y_d")],by.x="ID",by.y="Group.1",all.x=T)
reinforce$ref<- ifelse(reinforce[,c(2)]>0,reinforce[,c(2)],0)
ref_np[ref_np$ID==i,c("ref")] <-  as.numeric(colSums(reinforce[c("ref")]))
}
ref_np_0 <- ref_np   

##Creation of the raster for the grid costs for the exisiting infrastructure at t=0
ex_grid_cost <- merge(patchesNL[c("ID","pts_sel_IS","ID.y")],ref_np,by.x="ID.y",by.y="ID",all.x=T)
ex_grid_cost <- ex_grid_cost[order(ex_grid_cost$ID),]

cap_0 <- merge(patchesNL[c("ID","ID.y")],ref_np_0,by.x="ID.y",by.y="ID",all.x=T)
cap_0 <- cap_0[order(cap_0$ID),]

m_cap_0                                      <- round(t(matrix(cap_0$ref,nrow=31,ncol=23)),digits = 2)
m_cap_0                                      <- ifelse(is.na(m_cap_0),0,m_cap_0)
r_cap_0                                      <- raster(m_cap_0,xmn=-15,xmx=15,ymn=-11,ymx=11,crs="+proj=utm +units=m")    

#plot(r_cap_0)
NLSetPatches("cap_0",m_cap_0, nl.obj=nl.test1)

################################################################################
#                                                                              #
#                              MCSA                                            #
#                                                                              #
################################################################################

NLDoCommand(1,"setup-turtles",nl.obj=nl.test1)
NLDoCommand(1,"go",nl.obj=nl.test1)
NLDoCommand(1,"ask turtles with[breed = producers][die]",nl.obj=nl.test1)

#Grid Preparation
line_cap_IS   <- line_cap_t0 
sum_ref_IS    <- data.frame()
prodNL        <- NULL
pr_Col        <- data.frame()

repeat{ 
##Go
NLDoCommand(1,"go",nl.obj=nl.test1)

##Get Producer
prodNL         <-NLGetAgentSet(c("xcor","ycor","color"),"producers",nl.obj=nl.test1)

#Get Updated Patch Information
patchesUP      <-NLGetPatches(c("pxcor","pycor","phi_IS","phi_IS_MS1","phi_IS_MS3.1","city_id","y"),"patches",nl.obj=nl.test1)

pr_new         <-  prodNL[c(nrow(prodNL)),]
#prodNL     <- prodNL[prodNL$color == 15,c(1,2,3)]
  
      #Connect with patch infos
pr     <- merge(patchesNL[c("pxcor","pycor","ID", "pts_sel_IS","city_id","d_iv_IS","x_p","ID.y")],prodNL,by.x="pxcor",by.y="xcor")
pr     <- pr[pr$pycor == pr$ycor,]

pr     <- merge(patchesUP,pr,by.x="pxcor",by.y="pxcor")
pr     <- pr[pr$pycor.x == pr$pycor.y,]
 
      # Select and Connect new Producer per Tick
pr_new2        <-  pr
pr_new2$phi_IS <-  ifelse(pr_new2$pxcor == pr_new$xcor & pr_new$ycor== pr_new2$pycor.x,pr_new2$phi_IS,NA)
pr_new2        <- subset(pr_new2,!is.na(pr_new2$phi_IS),c(1:6))
pr_Col        <-  rbind(pr_Col,pr_new2)

# Criteria of Interruption
if(nrow(pr_Col[pr_Col$phi_IS < 0,]) > 0 ) {break} 

      #Splitt IS and OP Producer
pr_IS     <- pr[pr$color == 15,]
      #Calcualte supply 
Y_s_IS    <-  as.numeric(colSums(pr_IS[c("y")]))


#############Calculate DC-Model [Reinforced line sequements] IS#################
                                                     
      #Calculate generation form transformer station at t=1
gen_t    <- gen_y_d-as.numeric(colSums(pr_IS[c("y")]))/length(gen_y_d)

      #Create vector with generation cap and ID and select relevant PTDF큦
cp        <-  append(as.vector(na.omit(pr_IS$ID.y)),gen)
v_y_d     <-  append(as.vector(pr_IS$y),gen_t)
df_y_d    <-  as.data.frame(cbind(cp,v_y_d))
df_y_d    <-  aggregate(df_y_d,list(df_y_d$cp),sum)

ptdf_sel  <-  ptdf[c(paste(0,cp,sep="_"),paste(1,cp,sep="_"),paste(2,cp,sep="_"))]
      
      #Calculate line_cap of all transactions
line_c0  <- t((v_y_d * (Y_D_0 /(Y_D_0 + Y_D_1 + Y_D_2))) * t(ptdf_sel[,(paste(0,cp,sep="_"))]))
line_c1  <- t((v_y_d * (Y_D_1 /(Y_D_0 + Y_D_1 + Y_D_2))) * t(ptdf_sel[,(paste(1,cp,sep="_"))]))
line_c2  <- t((v_y_d * (Y_D_2 /(Y_D_0 + Y_D_1 + Y_D_2))) * t(ptdf_sel[,(paste(2,cp,sep="_"))]))
line_cap  <-  as.data.frame(rowSums(cbind(line_c0,line_c1,line_c2)))
     
      #Caluclation of the line sequments which have to be reinforced if x>0
reinforce                  <-  abs(line_cap) - abs(line_cap_t0) 
reinforce$ID               <-  as.numeric(row.names(reinforce))
reinforce                  <-  merge(reinforce,pts_net[c("ID","col1_r","col2_r","loads")],by="ID")
reinforce                  <-  merge(reinforce,df_y_d[c("Group.1","v_y_d")],by.x="ID",by.y="Group.1",all.x=T)
reinforce$ref              <- ifelse(reinforce[,c(2)]>0,reinforce[,c(2)],0)
ref                        <- as.data.frame(reinforce$ref)
tmp                        <- as.data.frame(ifelse(line_cap_IS > 0 ,line_cap_IS + ref,line_cap_IS - ref))
ref_IS                     <- as.numeric(colSums(reinforce[c("ref")]))
sum_ref_IS                 <- rbind(sum_ref_IS,ref_IS)
                              
ref_IS_M1                  <- ifelse(sec[sec$ID==iii,c("MS2")] == 0,as.numeric(colSums(sum_ref_IS[c(1)])),as.numeric(colSums(sum_ref_IS[c(1)])) * (1 - grid_ratio))
ref_IS_M2                  <- ifelse(sec[sec$ID==iii,c("MS2")] == 0,0,as.numeric(colSums(sum_ref_IS[c(1)])) * grid_ratio)

                            
#########Calculate DC-Model estimate line sequments by generation IS############
      
      #Calculate generation form transformer station at t=1     
gen_t    <- gen_y_d-as.numeric(colSums(pr_IS[c("y")]))/length(gen_y_d)

      #Create vector with generation cap and ID and select relevant PTDF큦
ref_np    <- data.frame(1:nrow(pts_net))
names (ref_np) [c(1)]  <- c("ID")

for (i in 1:nrow(pts_net))
{
cp        <-  append(as.vector(na.omit(pr_IS$ID.y)),gen)
cp        <-  append(cp,as.vector(i))
v_y_d     <-  append(as.vector(pr_IS$y),gen_t-sec[sec$ID==iii,c("h_max")]/length(gen_y_d))
v_y_d     <-  append(v_y_d,as.vector(sec[sec$ID==iii,c("h_max")]))
df_y_d    <-  as.data.frame(cbind(cp,v_y_d))
df_y_d    <-  aggregate(df_y_d,list(df_y_d$cp),sum)

ptdf_sel  <-  ptdf[c(paste(0,cp,sep="_"),paste(1,cp,sep="_"),paste(2,cp,sep="_"))]
      
      #Calculate line_cap of all transactions
line_c0  <- t((v_y_d * (Y_D_0 /(Y_D_0 + Y_D_1 + Y_D_2))) * t(ptdf_sel[,(paste(0,cp,sep="_"))]))
line_c1  <- t((v_y_d * (Y_D_1 /(Y_D_0 + Y_D_1 + Y_D_2))) * t(ptdf_sel[,(paste(1,cp,sep="_"))]))
line_c2  <- t((v_y_d * (Y_D_2 /(Y_D_0 + Y_D_1 + Y_D_2))) * t(ptdf_sel[,(paste(2,cp,sep="_"))]))
line_cap  <-  as.data.frame(rowSums(cbind(line_c0,line_c1,line_c2)))
     
      #Caluclation of the line sequments which have to be reinforced if x>0
reinforce    <-  abs(line_cap) - abs(line_cap_IS) 
reinforce$ID <-  as.numeric(row.names(reinforce))
reinforce    <-  merge(reinforce,pts_net[c("ID","col1_r","col2_r","loads")],by="ID")
reinforce    <-  merge(reinforce,df_y_d[c("Group.1","v_y_d")],by.x="ID",by.y="Group.1",all.x=T)
reinforce$ref<- ifelse(reinforce[,c(2)]>0,reinforce[,c(2)],0)
ref_np[ref_np$ID==i,c("ref")] <-  as.numeric(colSums(reinforce[c("ref")]))
}
ref_np_IS <- ref_np
# external Grid Cost
cap_IS <- merge(patchesNL[c("ID","ID.y")],ref_np_IS,by.x="ID.y",by.y="ID",all.x=T)
cap_IS <- cap_IS[order(cap_IS$ID),]

#############################Export to NetLogo##################################

#Instrument-Scenario
m_cap_IS                                      <- round(t(matrix(cap_IS$ref,nrow=31,ncol=23)),digits = 2)
m_cap_IS                                      <- ifelse(is.na(m_cap_IS),0,m_cap_IS)

## safety plot
#r_cap_IS                                      <- raster(m_cap_IS,xmn=-20,xmx=20,ymn=-14,ymx=14,crs="+proj=utm +units=m")    
#plot(r_cap_IS)

NLSetPatches("cap_IS",m_cap_IS, nl.obj=nl.test1)
NLCommand("set ref_IS_M1",ref_IS_M1, nl.obj=nl.test1)
NLCommand("set ref_IS_M2",ref_IS_M2, nl.obj=nl.test1)

## If Condition   Y_d >Y_s
if((Y_D) <= Y_s_IS ) {break}                   
}

################################################################################
#                                                                              #
#                        Evaluation preparation                                #
#                                                                              #
################################################################################

################################################################################
#                            NetLogo Imports                                   #
################################################################################

## Get Patches
patchesEND    <-NLGetPatches(c("pxcor","pycor","phi_IS","phi_IS_MS1","x_IS","y","city_id","d_iv_IS","x_p","cap_IS","h","x_s"),"patches",nl.obj=nl.test1)
##Get Producer
prodEND       <-NLGetAgentSet(c("xcor","ycor","color"),"producers",nl.obj=nl.test1)
#Get Reporter
s0   <- NLReport("s0",nl.obj=nl.test1)
s1   <- NLReport("s1",nl.obj=nl.test1)
s2   <- NLReport("s2",nl.obj=nl.test1)
t0   <- NLReport("t0",nl.obj=nl.test1)
t1   <- NLReport("t1",nl.obj=nl.test1)
t2   <- NLReport("t2",nl.obj=nl.test1)

#Merge
pr_END     <- merge(patchesEND,prodEND,by.x="pxcor",by.y="xcor")
pr_END     <- pr_END[pr_END$pycor == pr_END$ycor,]

pr_END$pr_c0  <- ifelse(pr_END$city_id== 0,pr$y,0)
pr_END$pr_c1  <- ifelse(pr_END$city_id== 1,pr$y,0)
pr_END$pr_c2  <- ifelse(pr_END$city_id== 2,pr$y,0)
      #Splitt IS and OP Producer
pr_END_IS     <- pr_END[pr_END$color == 15,]

# Calculate subsidies
pr_Col$S   <- 0
if (sec[sec$ID==iii,c("SU")] == 1)
{pr_Col$S   <- pr_Col$phi_IS - pr_Col$phi_IS_MS1
  if (sec[sec$ID==iii,c("MS3.1")] == 1)
  {pr_Col$S   <- pr_Col$phi_IS - pr_Col$phi_IS_MS3.1}
}
# Calculate taxes
pr_Col$T   <- 0 
if (sec[sec$ID==iii,c("EX")] == 1)
{pr_Col$T   <- pr_Col$phi_IS_MS1 - pr_Col$phi_IS
 if (sec[sec$ID==iii,c("MS3.1")] == 1)
  {pr_Col$T   <- pr_Col$phi_IS_MS3.1 - pr_Col$phi_IS}
}
# Negative Profit on a cell
pr_Col$yes <- ifelse(pr_Col$phi_IS < 0, 1,0)

yes <- 1
if (sum(pr_Col$yes) > 0)
{yes <- 0}


################################################################################
#                Caluclation of all welfare equation elements                  #
################################################################################       

#Energy Production
sc[sc$ID==ii,c("y_IS")] <- as.numeric(colSums(pr_END_IS[c("y")]))  
#Energy Production Cost
sc[sc$ID==ii,c("n_IS")] <- nrow(pr_END_IS) * w_PC * l
#Network Costs
sc[sc$ID==ii,c("C_GE_IS")] <- ref_IS_M1 *  w_GE + ref_IS_M2 *  w_GE
sc[sc$ID==ii,c("C_GC_IS")] <- as.numeric(colSums(pr_END_IS[c("d_iv_IS")]))  *  w_GC
#Producer Surplus
sc[sc$ID==ii,c("PS_IS")] <-  as.numeric(colSums(pr_Col[c("phi_IS")])) * Y_D / as.numeric(colSums(pr_END_IS[c("y")]))     
#Consumer Surplus
sc[sc$ID==ii,c("CS_IS")] <- Y_D * (p_max - p) / 2 + as.numeric(colSums(pr_END_IS[c("y")])) * m - ref_IS_M1 *  w_GE - sum(pr_Col[,c("S")])   
#External Costs
sc[sc$ID==ii,c("X_IS")]   <- as.numeric(colSums(pr_END_IS[c("x_IS")])) *  delta
sc[sc$ID==ii,c("X_P_IS")] <- as.numeric(colSums(pr_END_IS[c("x_p")])) *  delta
#Welfare
sc[sc$ID==ii,c("W_IS")] <- sc[sc$ID==ii,c("PS_IS")] + sc[sc$ID==ii,c("CS_IS")] - sc[sc$ID==ii,c("X_IS")]   
#Exit
sc[sc$ID==ii,c("EX_0_IS")] <-  NLReport("ex0",nl.obj=nl.test1)
sc[sc$ID==ii,c("EX_1_IS")] <-  NLReport("ex1",nl.obj=nl.test1)
sc[sc$ID==ii,c("EX_2_IS")] <-  NLReport("ex2",nl.obj=nl.test1)
#Subsidise
sc[sc$ID==ii,c("S_0_IS")] <-  sum(pr_Col[pr_Col$city_id==0,c("S")])
sc[sc$ID==ii,c("S_1_IS")] <-  sum(pr_Col[pr_Col$city_id==1,c("S")])
sc[sc$ID==ii,c("S_2_IS")] <-  sum(pr_Col[pr_Col$city_id==2,c("S")])
#Taxes
sc[sc$ID==ii,c("T_0_IS")] <-  sum(pr_Col[pr_Col$city_id==0,c("T")])
sc[sc$ID==ii,c("T_1_IS")] <-  sum(pr_Col[pr_Col$city_id==1,c("T")])
sc[sc$ID==ii,c("T_2_IS")] <-  sum(pr_Col[pr_Col$city_id==2,c("T")])
#Regional Consumer centre utility per Demand
sc[sc$ID==ii,c("CSX_0_IS")] <- (Y_D_0 * (p_max - p)/2 + sum(pr_END_IS[pr_END_IS$city_id==0,c("y")])* m - sum(pr_END_IS[pr_END_IS$city_id==0,c("x_IS")]) * delta - ref_IS_M2 *  w_GE * Y_D_0 / Y_D - sc[sc$ID==ii,c("S_0_IS")]) / Y_D_0  
sc[sc$ID==ii,c("CSX_1_IS")] <- (Y_D_1 * (p_max - p)/2 + sum(pr_END_IS[pr_END_IS$city_id==1,c("y")])* m - sum(pr_END_IS[pr_END_IS$city_id==1,c("x_IS")]) * delta - ref_IS_M2 *  w_GE * Y_D_1 / Y_D - sc[sc$ID==ii,c("S_1_IS")]) / Y_D_1  
sc[sc$ID==ii,c("CSX_2_IS")] <- (Y_D_2 * (p_max - p)/2 + sum(pr_END_IS[pr_END_IS$city_id==2,c("y")])* m - sum(pr_END_IS[pr_END_IS$city_id==2,c("x_IS")]) * delta - ref_IS_M2 *  w_GE * Y_D_2 / Y_D - sc[sc$ID==ii,c("S_2_IS")]) / Y_D_2  
#Regional Spatial Externality per Demand
sc[sc$ID==ii,c("X_0_IS")] <- sum(pr_END_IS[pr_END_IS$city_id==0,c("x_IS")]) * delta / Y_D_0
sc[sc$ID==ii,c("X_1_IS")] <- sum(pr_END_IS[pr_END_IS$city_id==1,c("x_IS")]) * delta / Y_D_1
sc[sc$ID==ii,c("X_2_IS")] <- sum(pr_END_IS[pr_END_IS$city_id==2,c("x_IS")]) * delta / Y_D_2
#Regional utility per Demand
sc[sc$ID==ii,c("W_0_IS")] <- sc[sc$ID==ii,c("CSX_0_IS")] + (as.numeric(sum(pr_Col[pr_Col$city_id == 0,c("phi_IS")])) * Y_D / as.numeric(colSums(pr_END_IS[c("y")])) - ref_IS_M2 *  w_GE * Y_D_0 / Y_D) / Y_D_0    
sc[sc$ID==ii,c("W_1_IS")] <- sc[sc$ID==ii,c("CSX_1_IS")] + (as.numeric(sum(pr_Col[pr_Col$city_id == 1,c("phi_IS")])) * Y_D / as.numeric(colSums(pr_END_IS[c("y")])) - ref_IS_M2 *  w_GE * Y_D_1 / Y_D) / Y_D_1   
sc[sc$ID==ii,c("W_2_IS")] <- sc[sc$ID==ii,c("CSX_2_IS")] + (as.numeric(sum(pr_Col[pr_Col$city_id == 2,c("phi_IS")])) * Y_D / as.numeric(colSums(pr_END_IS[c("y")])) - ref_IS_M2 *  w_GE * Y_D_2 / Y_D) / Y_D_2  
#Gini Coefficient
tmp <- c(sc[sc$ID==ii,c("W_0_IS")],sc[sc$ID==ii,c("W_1_IS")],sc[sc$ID==ii,c("W_2_IS")]) 
tmp <-   (tmp - min(tmp)) /  (max(tmp)- min(tmp))  
sc[sc$ID==ii,c("GINI_CSX_IS")] <- ineq(tmp,type="Gini") * 1.5
}

################################################################################
#               Combining Calculations with applied parameter                  #
################################################################################
  
# Complement Landscape Information
dat_<- sc
dat_$ID       <- sec[sec$ID==iii,c("ID_S")]
dat_$w_PC     <- w_PC
dat_$w_GC     <- w_GC
dat_$delta    <- delta
dat_$l        <- l
dat_$m        <- m
dat_$p_max    <- p_max
dat_$w_GE     <- w_GE
dat_$theta    <- theta
dat_$delta    <- delta
dat_$ID_S     <- sec[sec$ID==iii,c("ID_S")]
dat_$ID_L     <- ID_L
dat_$lui      <- sec[sec$ID==iii,c("lui")]
dat_$x_s_max  <- x_s_max
dat_$h_max    <- h_max
dat_$alfa     <- alfa
dat_$beta     <- beta
dat_$grid_r   <- grid_ratio
dat_$lu_res   <- lu_res
dat_$lui_d    <- lui_d_sec
dat_$lui_s    <- lui_s_sec
dat_$lui_h    <- lui_h_sec
dat_$lui_x    <- lui_x_sec
dat_$psi      <- psi
dat_$lambda   <- lambda
dat_$gamma    <- gamma
dat_$s        <- s
dat_$t        <- s

# City Values
dat_$Y_D_0    <-  Y_D_0
dat_$Y_D_1    <-  Y_D_1
dat_$Y_D_2    <-  Y_D_2
dat_$h_mean   <-  mean(patchesEND[patchesEND$d_iv_IS != 999 ,c("h")])
dat_$h_0      <- mean(patchesEND[patchesEND$d_iv_IS != 999 & patchesEND$city_id == 0,c("h")]) / mean(patchesEND[patchesEND$d_iv_IS != 999 ,c("h")])
dat_$h_1      <- mean(patchesEND[patchesEND$d_iv_IS != 999 & patchesEND$city_id == 1,c("h")]) / mean(patchesEND[patchesEND$d_iv_IS != 999 ,c("h")])
dat_$h_2      <- mean(patchesEND[patchesEND$d_iv_IS != 999 & patchesEND$city_id == 2,c("h")]) / mean(patchesEND[patchesEND$d_iv_IS != 999 ,c("h")])
dat_$x_s_mean <-  mean(patchesEND[patchesEND$d_iv_IS != 999 ,c("x_s")])
dat_$x_s_0    <- mean(patchesEND[patchesEND$d_iv_IS != 999 & patchesEND$city_id == 0,c("x_s")]) / mean(patchesEND[patchesEND$d_iv_IS != 999 ,c("x_s")])
dat_$x_s_1    <- mean(patchesEND[patchesEND$d_iv_IS != 999 & patchesEND$city_id == 1,c("x_s")]) / mean(patchesEND[patchesEND$d_iv_IS != 999 ,c("x_s")])
dat_$x_s_2    <- mean(patchesEND[patchesEND$d_iv_IS != 999 & patchesEND$city_id == 2,c("x_s")]) / mean(patchesEND[patchesEND$d_iv_IS != 999 ,c("x_s")])
dat_$s0   <- s0
dat_$s1   <- s1
dat_$s2   <- s2
dat_$t0   <- t0
dat_$t1   <- t1
dat_$t2   <- t2


################################################################################
#                 Testing the model run plausibilty                            #
################################################################################

## plausibilty criteria 
if (dat_$W_IS < 0 & sec[sec$ID==iii,c("ID_S")] == 1) 
{yes <- 0}
if (yes == 1)
{dat <-rbind(dat,dat_)}
if (yes == 0)
{nodat <-rbind(nodat,dat_)}

save(dat,file=paste(lib,'RESAM-sample.RData',sep=""))  
save(nodat,file=paste(lib,'Residuals.RData',sep=""))

}
## Stop when enoug samples have been collected
if (nrow(dat) == 0)
{tmp <- data.frame(1:1)}
if (nrow(dat) != 0)
{tmp <- na.omit(reshape(dat[,c("ID_L","ID_S")],v.names="ID_S",idvar="ID_L",timevar="ID_S",direction="wide"))}


if(nrow(tmp) == samplesize ) {break} 

show(nrow(tmp))
show(Sys.time())
}
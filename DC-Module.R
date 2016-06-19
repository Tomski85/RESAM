
################################################################################
#                                                                              #
#                                PTDF                                          #
#                    (Power Transfer Distribution Matrix)                      #
################################################################################


################################################################################
#                 Import from NetLogo to R                                     #
################################################################################

#Pushing the setup bottom within NetLogo
NLCommand("setup", nl.obj=nl.test1)

#Import NetLogo data
patchesSt <-NLGetPatches(c("pxcor","pycor","y"),"patches",nl.obj=nl.test1)
citiesNL <-NLGetAgentSet(c("who","xcor","ycor"),"cities",nl.obj=nl.test1)

################################################################################
#             Data Preparation for creating the Graph                          #
################################################################################

Y_D_0 <- 50
Y_D_1 <- 50
Y_D_2 <- 50
Y_D   <- Y_D_0 + Y_D_1 + Y_D_2

patches <- patchesSt
#convert to SpatialPointDataFrame
coordinates(patches)  <- ~pxcor+pycor
#convert to SpatialPixelDataFrame
gridded(patches) <- TRUE

#Generate matrix
m_x_s            <- round(t(as.matrix(patches[1])),digits = 2)
# Creating a matrix as raster generalization of the virtual landscape
m_1              <- ifelse(is.na(m_x_s),1,1)
# Creating a raster
r_1                <- raster(m_1,xmn=-15,xmx=15,ymn=-11,ymx=11,crs="+proj=utm +units=m")  
#Create TransitionMatrix with 8 directions
tr_1         <- transition(1/r_1,transitionFunction=mean, directions=8) 
#Correction of the matrix because of diagonal movement
trc_1       <- geoCorrection(tr_1,type="c",multpl=F, scl=T) 
## create citie points
citie_p      <- as.matrix(citiesNL[c(2,3)])

################################################################################
#          Creating the coordinates of the existing grid structure             #
################################################################################

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

##get graph out of network
pts_net    <- data.frame(pts_sel_1)
pts_net$ID <- seq_len(nrow(pts_net))

#Visualization of the ID´s
plot(pts_sel_1)
text(pts_sel_1[,1], pts_sel_1[,2], 1:nrow(pts_sel_1))

##table preparation

names(pts_net)[c(1:2)] <- c("GeoX","GeoY")
pts_net <- pts_net[c(3,1,2)]

################################################################################
#          Defining the electricity movement with the existing grid system     #
################################################################################

# edges anticlockwise 
pts_net$col1_l <- pts_net$ID
pts_net$col2_l <- ifelse(pts_net$col1_l >= 16, pts_net$col1_l + 1,NA)
pts_net$col2_l <- ifelse(pts_net$col1_l >= 2 & pts_net$col1_l <= 14 & pts_net$col1_l%%2 ==0, pts_net$col1_l + 2,pts_net$col2_l)
pts_net$col2_l <- ifelse(pts_net$col1_l >= 3 & pts_net$col1_l <= 15 & pts_net$col1_l%%2 !=0, pts_net$col1_l - 2,pts_net$col2_l)
pts_net$col2_l <- ifelse(pts_net$col1_l == 1 ,2,pts_net$col2_l)
pts_net$col2_l <- ifelse(pts_net$col1_l == 32 ,15,pts_net$col2_l)

# edges clockwise
pts_net$col1_r <- pts_net$ID
pts_net$col2_r <- ifelse(pts_net$col1_l >= 17, pts_net$col1_l - 1,NA)
pts_net$col2_r <- ifelse(pts_net$col1_l >= 4 & pts_net$col1_l <= 16 & pts_net$col1_l%%2 ==0, pts_net$col1_l - 2,pts_net$col2_r)
pts_net$col2_r <- ifelse(pts_net$col1_l >= 1 & pts_net$col1_l <= 15 & pts_net$col1_l%%2 !=0, pts_net$col1_l + 2,pts_net$col2_r)
pts_net$col2_r <- ifelse(pts_net$col1_l == 2 ,1,pts_net$col2_r)
pts_net$col2_r <- ifelse(pts_net$col1_l == 15 ,32,pts_net$col2_r)

# Defining Loads (Consumer Centres) and Generation
pts_net$names <- "connection point"
pts_net$names <- ifelse(pts_net$ID == 1,"city 0",pts_net$names)
pts_net$names <- ifelse(pts_net$ID == 32,"city 1",pts_net$names)
pts_net$names <- ifelse(pts_net$ID == 16,"city 2",pts_net$names)
pts_net$names <- ifelse(pts_net$ID == 24 ,"transformer station",pts_net$names)

# weights (vertical or diagonal distance)
a <- sqrt(2)
pts_net$weight_l <- ifelse(pts_net$col1_l <= 16 & pts_net$col2_l <= 16, (a) ,1)
pts_net$weight_r <- ifelse(pts_net$col1_r <= 15 & pts_net$col2_r <= 15, a ,1)
pts_net$weight_r <- ifelse(pts_net$col1_r == 16 & pts_net$col2_r == 14, a ,pts_net$weight_r)
pts_net$weight_r <- ifelse(pts_net$col1_r == 15 & pts_net$col2_r == 32, a ,pts_net$weight_r)
pts_net$weight_l <- ifelse(pts_net$col1_l == 32 & pts_net$col2_l == 15, (a) ,pts_net$weight_l)

total_imp <- sum(pts_net$weight_r)

# loads
pts_net$loads <- ifelse(pts_net$ID == 1, Y_D_0,0)                  # Implement Link to NetLogo Turtle citie 0
pts_net$loads <- ifelse(pts_net$ID == 32, Y_D_1,pts_net$loads)     # Implement Link to NetLogo Turtle citie 1
pts_net$loads <- ifelse(pts_net$ID == 16, Y_D_2,pts_net$loads)     # Implement Link to NetLogo Turtle citie 2

# Generation 
pts_net$generation <- ifelse(pts_net$ID == 24, Y_D,0)

################################################################################
#              Creating two graphs for every direction                         #
################################################################################

#Vertices
g_net_r <- graph.empty()
#g_net_l <- graph.empty()
g_net_r <- add.vertices(g_net_r, nrow(pts_net), ID=pts_net[,1], names=pts_net[,8], loads=pts_net[,11], generation=pts_net[,12])
#g_net_l <- add.vertices(g_net_l, nrow(pts_net), ID=pts_net[,1], names=pts_net[,8], loads=pts_net[,9], generation=pts_net[,10])

#Edges
edges_r                   <- pts_net[c("col1_r","col2_r","weight_r","names")] 
names(edges_r)[c(1:3)]<- c("col1","col2","weight")
edges_r$direction         <- "r"

edg                       <- edges_r[c(1:2)]
g_net_r                   <- add.edges(g_net_r, t(edg),weight=edges_r[,3],direction=edges_r[,5],names=edges_r[,4])

#Arrange Informations of graphs
g_net_df_edg   <-get.data.frame(g_net_r,what="edges")
g_net_df_ver   <-get.data.frame(g_net_r, what="vertices")


################################################################################
#             Calculation of the Impedance from the Distance                   #
################################################################################

#shortest path clockwiese all vertices to the cities 
t_d_c <- data.frame(1:32)
for (ii in c("0","1","2"))
{
  t_end <- data.frame()
  for (i in 1:32){
    t_1 <- data.frame(shortest.paths(g_net_r, v=V(g_net_r)[ID == i], to=V(g_net_r)[names == paste("city",ii,sep=" ")],mode = c("in"),weights = NULL, algorithm = c("automatic")))
    names (t_1) [c(1)]  <- c(paste("r_d_c_",ii,sep=""))
    t_1$ID <- i
    t_end <- rbind(t_end,t_1)
  }
  t_d_c <- cbind(t_d_c,t_end)
}
t_d_c_r <- t_d_c[c("ID","r_d_c_0","r_d_c_1","r_d_c_2")]


################################################################################
#          Preparation of the Power Transfer Distribution Factor Matrix        #
################################################################################

# Preparation of the Power Transfer Distribution Matrix

total_imp      <- sum(pts_net$weight_r)
therehold_imp  <-round(total_imp/2,digits=8)


t_d_c_r$r_imp_c0 <- (t_d_c_r$r_d_c_0)/ total_imp 
t_d_c_r$r_imp_c1 <- (t_d_c_r$r_d_c_1)/ total_imp 
t_d_c_r$r_imp_c2 <- (t_d_c_r$r_d_c_2)/ total_imp 

# Calculation of the route values over the route path

r_imp_city     <- t_d_c_r[c("ID","r_imp_c0","r_imp_c1","r_imp_c2")]
r_edges        <-get.data.frame(g_net_r,what="edges")
r_edges$ID     <- r_edges$from
r_edges        <- r_edges[c("ID","from","to","names")]

# Calculation of the Power Transfer Distribution Factor Matrix

ptdf        <- r_edges[c("ID","from","to")]

for (t in 1:nrow(r_edges))
{         
  c <- r_edges[c("ID")]
  for (f in c("0","1","2"))
  {
    a <- get.shortest.paths(g_net_r, from=V(g_net_r)[names == paste("city",f,sep=" ")], to=V(g_net_r)[ID == t], mode = c("in"), weights = NULL, output=c("vpath"))
    a <- data.frame(as.numeric(a$vpath[[1]]))
    names (a) [c(1)]       <- "pass"
    b                      <- merge(r_edges[c("ID","from","to")],a[c("pass","pass")],by.x="ID",by.y="pass",all.x=T )
    b$ptdf                 <- ifelse(is.na(b$pass.1),r_imp_city[r_imp_city$ID==t,c(paste("r_imp_c",f,sep=""))]-1,r_imp_city[r_imp_city$ID==t,c(paste("r_imp_c",f,sep=""))])
    b$ptdf                 <- ifelse(b$ptdf == -1,0,b$ptdf)
    names(b)[c(5)]         <- c(paste(f,t,sep="_"))
    c                      <- cbind(c,b[c(paste(f,t,sep="_"))])
  }
  ptdf                   <- cbind(ptdf,c[c(2,3,4)])
}



###Checking Graph
#V(g_net_r)$names
#V(g_net_r)$ID
#V(g_net_r)$loads
#V(g_net_r)$generation
#
#E(g_net_r)

#
##Visulaisation with diferent edge colors 
#E(g_net_r)$color <- "black"
#E(g_net_r)[ weight==1 ]$color <- "red"
#V(g_net_r)$color <- "grey"
#V(g_net_r)[ loads>0 ]$color <- "light blue"
#V(g_net_r)[ generation>0 ]$color <- "red"
#tkplot(g_net_r,edge.color=E(g_net_r)$color,vertex.color=V(g_net_r)$color)


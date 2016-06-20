################################################################################
#                         Model-Setup                                          #
################################################################################

####################### loading packages #######################################

library(RNetLogo)
library(raster)
library(gdistance)
library(igraph)
library(ineq)

####################### Defining workspace #####################################

lib           <- 'Y:\\Home\\lauft\\_Promotion_\\Software\\Github Folder\\'
NetLogo_path  <- "C:/Program Files (x86)/NetLogo 5.0.4"

################ loading functions and external data ###########################

# Functions
source(paste(lib,'R-Modules\\Functions\\RasterFunctions.r',sep=""))

######################## Open NetLogo ##########################################

nl.test1 <- "nl.test1"
NLStart(NetLogo_path, gui=T, nl.obj=nl.test1)

#Open model
NLLoadModel(paste(lib,'NetLogo-Modules\\RESAM.nlogo',sep=""),nl.obj=nl.test1)

################################################################################
#                        Scenario Definition                                   #
################################################################################

#### Defining the number of scenarios and the sample size
samplesize     <- 10
n              <- 3 

sec            <- data.frame(1:n) 
names(sec)[1]  <- "ID"

# Defining the IDs of the scenarios
sec$ID_S       <- c(1,16,17) 
sec$ID_L       <- c(1)


### Defining the policies

# Market-based polices [0 = no , 1 = yes]
sec$MS2         <- c(0)    # Reinforcement cost share to producer
sec$MS3         <- c(0)    # Reference yield model
sec$MS3.1       <- c(1)    # Reference yield model with boundaries

# Regulation polices
sec$lui_d      <- c(0)     # Maximum distance related land-use regulation in %
sec$lui_s      <- c(0)     # Maximum site related land-use regulation in %
sec$lui_h      <- c(100)   # Minimum harvest dependent land-use designation in %
sec$lui_ex     <- c(50)    # Minimum spatial externalities dependent land-use designation in %

### Defining the coalition structure [0 = no , 1 = yes]
sec$CO         <- c(1)     # Do we have a grand coalition of all consumer centres
sec$PA         <- c(0)     # Which coalition form will be applied

### Defining the consumer centre strategies [0 = no , 1 = yes]
sec$EX         <- c(0,1,0) # Can the consumer centre apply the exit strategy
sec$SU         <- c(0,0,1) # Can the consumer centre apply the support strategy

### Defining the parameter interval boundaries

# landscape parameter
sc        <- data.frame(1:1)
names(sc)[1] <- "ID"

# Producer surplus related parameters
sc$h_max    <- c(list(c(4,6)))         # Maximum energetic yield potential
sc$l        <- c(list(c(1.56,2.34)))   # Plant´s performance parameter
sc$w_PC     <- c(list(c(2.4,3.6)))     # Factor price of the respective plant type

# Consumer surplus related parameters
sc$Y_D_0    <- c(list(c(50,50)))       # Demand of the consumer centre 1 
sc$Y_D_1    <- c(list(c(50,50)))       # Demand of the consumer centre 2 
sc$Y_D_2    <- c(list(c(50,50)))       # Demand of the consumer centre 3 
sc$p        <- c(1)                    # Price for electricity
sc$p_max    <- c(list(c(1.2,1.8)))     # Consumers' willingness to pay
sc$m        <- c(list(c(0.24,0.36)))   # Regional multiplier effect
sc$w_GE     <- c(list(c(0.044,0.066))) # Reinforcement related factor price for existing grid

# External spatial cost parameters
sc$x_s_max  <- c(list(c(40,60)))       # Maximum site-dependent external costs
sc$alfa     <- c(list(c(80,120)))      # Intensity parameter 
sc$beta     <- c(list(c(0.40,0.60)))   # Curvature parameter
sc$delta    <- c(list(c(0.04,0.06)))   # External spatial costs parameter
sc$theta    <- c(list(c(0.04,0.06)))   # Ratio between transport and production related external spatial costs parameter

# Grid Setting
sc$gen      <- c(list(c(2,15,17)))     # Position of the inital convential power plants per consumer centre

################################################################################
#                          Run the model                                       #
################################################################################

# Running the grid module
source(paste(lib,'R-Modules\\Grid-Module.r',sep=""))

# Running the main module
source(paste(lib,'R-Modules\\Main-Module.r',sep=""))

























##############
# Generate a random density distribution
#    following a scaling law with params alpha>0, Pmax (P_i = Pmax*i^{-\alpha})
#  
#  Value :
#     Spatial matrix of distribution values
#
#  Parameters :
#     - gridSize : width of the square grid
#     - N : number of kernels (cities)
#     - alpha : rank-size scaling parameter
#     - proba = TRUE : return a probability distribution
#     - rmin = 0 : minimal radius of cities (uniformaly drawn within rmlin,rmax) ; gridSize/N if 0
#     - rmax = 0 : idem
#     - Pmax = 1 : population of the greatest city (useful if proba == FALSE)
#     - tolThreshold : DEPRECATED
#     - kernel_type = "poisson" : can be "gaussian" (sigma=1/(2*r^2)) or "poisson" (sigma=1/r)

spatializedExpMixtureDensity <- function(gridSize,N,alpha,centerDensity=0.005,proba=TRUE,rmin=0,rmax=0,Pmax=1,tolThreshold=0,kernel_type="poisson"){
  
  #if(rmin==0){rmin = gridSize/N}
  #if(rmax==0){rmax = gridSize/N}
  
  # patches of the grid are 1 unit size (in r_min/max units)
  grid = matrix(0,gridSize,gridSize)
  
  # matrix of coordinates
  coords = matrix(c(c(matrix(rep(1:gridSize,gridSize),nrow=gridSize)),c(matrix(rep(1:gridSize,gridSize),nrow=gridSize,byrow=TRUE))),nrow=gridSize^2)
  
  # first draw param distribs ? not needed
  # for exp distribs, P_i = 2pi*d_i*r_i^2
  #  -> take P from deterministic distrib ; draw r.
  
  for(i in 1:N){
    #show(i)
    pop_i = Pmax*i^{-alpha}
    d_i = centerDensity
    r_i = sqrt(pop_i/(2*pi*d_i))
    #show(r_i)
    #r_i = runif(1,min=rmin,max=rmax)
    #d_i = pop_i / (2*pi*(r_i^2))
    
    # find origin of that kernel
    #  -> one of points such that : d(bord) > rcut and \forall \vec{x}\in D(rcut),d(\vec{x})<tolThr.
    #pot = which(!pseudoClosing(grid>tolThreshold,r_i),arr.ind=TRUE)
    #show(length(pot))
    #if(length(pot)==0){
    #  # Take a point with minimal density ?
    #  pot = which(grid==min(grid),arr.ind=TRUE)
    #}
    
    # simplify : take deterministiquely (almost, after two exps only two points possible)
    # BUT not close to border
    #rbord = 2*rmax*log(Pmax/tolThreshold)
    #rbord = 2*rmax
    
    # random center
    # if(max(grid)==0){
    #   # random if no center yet
    #   center = matrix(runif(2,min=rbord+1,max=gridSize-rbord),nrow=1)
    # }
    # else {
    #   # else find min pop area not too close to border
    #   pot = which(grid==min(grid[(rbord+1):(gridSize-rbord),(rbord+1):(gridSize-rbord)]),arr.ind=TRUE)
    #   row = sample(nrow(pot),1)
    #   center = matrix(pot[row,],nrow=1)
    # }
    
    center = matrix(runif(2,min=1,max=gridSize),nrow=1)
    
    # add kernel : use kernlab laplace kernel or other
    #if(kernel_type=="poisson"){ker=laplacedot(sigma=1/r_i)}
    #if(kernel_type=="gaussian"){ker=rbfdot(sigma=1/(2*r_i^2))}
    #if(kernel_type="quadratic"){ker=} # is quad kernel available ?
    
    grid = grid + (d_i * matrix(kernelMatrix(kernel=laplacedot(sigma=1/r_i),x=coords,y=center),nrow=gridSize))
    
  }
  
  if(proba==TRUE){grid = grid / sum(grid)}
  
  return(grid)
}


spatializedExpMixtureDensity2 <- function(gridSize,N,alpha,proba=TRUE,rmin=0,rmax=0,Pmax=1,tolThreshold=0,kernel_type="poisson"){
  
  if(rmin==0){rmin = gridSize/N}
  if(rmax==0){rmax = gridSize/N}
  
  # patches of the grid are 1 unit size (in r_min/max units)
  grid = matrix(0,gridSize,gridSize)
  
  # matrix of coordinates
  coords = matrix(c(c(matrix(rep(1:gridSize,gridSize),nrow=gridSize)),c(matrix(rep(1:gridSize,gridSize),nrow=gridSize,byrow=TRUE))),nrow=gridSize^2)
  
  # first draw param distribs ? not needed
  # for exp distribs, P_i = 2pi*d_i*r_i^2
  #  -> take P from deterministic distrib ; draw r.
  
  for(i in 1:N){
    #show(i)
    pop_i = Pmax*i^{-alpha}
    r_i = runif(1,min=rmin,max=rmax)
    d_i = pop_i / (2*pi*(r_i^2))
    
    # find origin of that kernel
    #  -> one of points such that : d(bord) > rcut and \forall \vec{x}\in D(rcut),d(\vec{x})<tolThr.
    #pot = which(!pseudoClosing(grid>tolThreshold,r_i),arr.ind=TRUE)
    #show(length(pot))
    #if(length(pot)==0){
    #  # Take a point with minimal density ?
    #  pot = which(grid==min(grid),arr.ind=TRUE)
    #}
    
    # simplify : take deterministiquely (almost, after two exps only two points possible)
    # BUT not close to border
    rbord = 2*rmax*log(Pmax/tolThreshold)
    #rbord = 2*rmax
    
    # random center
    if(max(grid)==0){
      # random if no center yet
      center = matrix(runif(2,min=rbord+1,max=gridSize-rbord),nrow=1)
    }
    else {
      # else find min pop area not too close to border
      pot = which(grid==min(grid[(rbord+1):(gridSize-rbord),(rbord+1):(gridSize-rbord)]),arr.ind=TRUE)
      row = sample(nrow(pot),1)
      center = matrix(pot[row,],nrow=1)
    }
    
    # add kernel : use kernlab laplace kernel or other
    if(kernel_type=="poisson"){ker=laplacedot(sigma=1/r_i)}
    if(kernel_type=="gaussian"){ker=rbfdot(sigma=1/(2*r_i^2))}
    #if(kernel_type="quadratic"){ker=} # is quad kernel available ?
    
    grid = grid + (d_i * matrix(kernelMatrix(kernel=laplacedot(sigma=1/r_i),x=coords,y=center),nrow=gridSize))
    
  }
  
  if(proba==TRUE){grid = grid / sum(grid)}
  
  return(grid)
}



jointdis <- function(company, interpartner, SDproduct){
  a <- rnorm(1000,Dataframe$Inputdist[i],SDproduct)
  b <- rnorm(1000, Dataframe$Outputdis[interpartner],SDproduct)
  # define limits of a common grid, adding a buffer so that tails aren't cut off
  lower <- min(c(a, b)) - 1 
  upper <- max(c(a, b)) + 1
  
  # generate kernel densities
  da <- density(a, from=lower, to=upper)
  db <- density(b, from=lower, to=upper)
  d <- data.frame(x=da$x, a=da$y, b=db$y)
  
  # calculate intersection densities
  d$w <- pmin(d$a, d$b)
  
  # integrate areas under curves
  total <- integrate.xy(d$x, d$a) + integrate.xy(d$x, d$b)
  intersection <- integrate.xy(d$x, d$w)
  
  # compute overlap coefficient
  overlap <- 2 * intersection / total
  return(overlap)
}

inputgen <- function(geo.distances= geo.distances, lengthcor= lengthcor, Nb.companies=Nb.companies){
Corlist <- list()
dataa <- matrix(c(rep(0, Nb.companies*Nb.companies)), ncol=Nb.companies)
for(i in 1:50){
  
  n     <- Nb.companies                    # length of vector
  rho   <- lengthcor      # desired correlation = cos(angle)
  theta <- acos(rho)             # corresponding angle
  x1    <- geo.distances[i,]        # fixed given data
  x2    <- rnorm(50, 0, 10)      # new random data
  X     <- cbind(x1, x2)         # matrix
  Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
  
  Id   <- diag(n)                               # identity matrix
  Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
  P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
  x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
  Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
  Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
  
  x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
  dataa[i,] <- x
}
dataa[lower.tri(dataa)]<-0
lcor <- mean(cor(geo.distances[,], colMeans(dataa)))
Corlist <- list(dataa,lcor)
return(Corlist)
}

SampleCompanies <- function(SpatialDensity=AA, Nb.companies, gridSize){
HH <- as.vector(SpatialDensity)
BB <- (c(1:(gridSize*gridSize)))
FF<-sample(BB, Nb.companies, prob= HH, replace=F)
for(ii in 1:Nb.companies){
  for(i in 1:(gridSize*gridSize)){
  if(BB[i] == FF[ii]){BB[i] <- T}
}
}
BB<- ifelse(BB != 1, 0,1)
BB[1]<- 0
sum(BB)
JJ<-matrix(BB, ncol=100, byrow=T)
LL <-which(JJ == 1, arr.ind=T)
#plot(LL)
return(LL)
}

#persp3D(z=CC)



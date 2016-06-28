
### Script for the second version of the circular economy model – same logic but slightly different modelling approach

### For the moment the code is quite basic, just randomly placing comanies in space and matching pairs of them based on geographic distance and probaiblity of one buying the waste of another (inspired by the niche modelling approach of Williams et al. 2010)

# Parameters
Nb.companies <- 100
dist.decay <- 0.2

# Place companies at random in space
lat <- runif(Nb.companies, min = 0, max = 1)
long <- runif(Nb.companies, min = 0, max = 1)

# Compute geographical distance between pairs of companies
geo.distances <- as.matrix(dist(cbind(lat, long), upper = T, diag=T))
interaction.proba <- exp(-(geo.distances/dist.decay))
diag(interaction.proba) = 0

# Compute the probability that company A buys waste from company B 
waste.value <- runif(Nb.companies, min = 0, max = 1)
demand.optimum <- runif(Nb.companies, min = 0, max = 1)
demand.range <- runif(Nb.companies, min = 0, max = 1)

buying_proba <- function(nj, ci, ri){ return(1 * exp(-((nj - ci)/(ri / 2))^2)) }

waste.buying.proba <- matrix(nrow=length(waste.value), ncol=length(waste.value))
for(i in 1:length(waste.value)){
	for(j in 1:length(waste.value)){
		waste.buying.proba[i,j] <- buying_proba(waste.value[j], demand.optimum[i], demand.range[i])
}}
diag(waste.buying.proba) = 0


# Algorithm to match companies
companies.need <- 1:Nb.companies
companies.waste <- 1:Nb.companies
waste.buying.proba2 <- waste.buying.proba
interaction.proba2 <- interaction.proba
links = list()
i=1
while(length(dim(waste.buying.proba2)[1] > 0)){
	need <- sample(1:dim(waste.buying.proba2)[1],1)
	link <- c(need, which.max(waste.buying.proba2[need,] + interaction.proba2[need,]))
	interaction.proba2 <- interaction.proba2[-need, -link[2]]
	waste.buying.proba2 <- waste.buying.proba2[-need, -link[2]]
	links[[i]] <- c(companies.need[need], companies.waste[link[2]])
	companies.need <- companies.need[-need]
	companies.waste <- companies.waste[-link[2]]
	i = i+1
}

# Plot the companies in space and their connexion (i.e. transfer of waste)
plot(cbind(long,lat), pch=20)
for(i in 1:Nb.companies){
	arrows(long[links[[i]][1]], lat[links[[i]][1]], long[links[[i]][2]], lat[links[[i]][2]])
}






library(markovchain)
library(ctmcd)
library(diagram)
library(pracma)
library(Matrix)
library(expm)




# Exercice 1.
# On considère la chaîne à temps continu sur l’espace {1,2,3}, de générateur infinitésimal A.


stateNames <- c("1","2","3")


# Construire sous R un générateur infinitésimal 3x3.

GeneratorMatrix <- matrix(c(-2,1,1
                            ,1/2,-1,1/2
                            ,0,1/3,-1/3),
                         nrow=3, byrow=TRUE)
row.names(GeneratorMatrix) <- stateNames; colnames(GeneratorMatrix) <- stateNames
round(GeneratorMatrix,3)



# Checking the transition matrix of our Generator matrix using generatorToTransitionMatrix function from markovchain library

TransitionMatrix <- generatorToTransitionMatrix(GeneratorMatrix)
TransitionMatrix

# we can plot or transition matrix

plotmat(TransitionMatrix,pos = c(1,2),
        lwd = 1, box.lwd = 2,
        cex.txt = 0.8,
        box.size = 0.1,
        box.type = "circle",
        box.prop = 0.5,
        box.col = "light yellow",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .13,
        main = "")


# En diagonalisant A
# Checking if our matrix can be diagnolized

diagflag = function(m,tol=1e-10){
  x = eigen(m)$vectors
  y = min(abs(eigen(x)$values))
  return(y>tol)
}

diagflag(GeneratorMatrix)


# calculer e
# brute force solution by using a highr exp of 100 to reach steady state

P <- function(t) {expm(t*GeneratorMatrix)}
expMatrix <- P(100)
round(expMatrix,3)



# Trouver la loi stationnaire de A.
# Using the steadyStates function from the markovchain library

CTMC_Object <- new("ctmc", states = stateNames,
                     byrow = TRUE, generator = GeneratorMatrix,)

steadyStates(CTMC_Object)




# Exercice 2.

# Écrire une fonction MMs qui mesure les performances d’une file d’attente M/M/s.


rm(list=ls())

mms_performance <- function(lambda, mu, n, s){
  Utilization <- (lambda/mu)/n
  Average_num_of_customers_in_the_system <- lambda/(mu-lambda)
  Average_num_of_customers_in_the_queue <- Utilization * Average_num_of_customers_in_the_system
  Average_time_a_customer_spends_in_the_system <- n / (mu-lambda)
  Average_time_a_customer_spends_in_the_queue <- Average_num_of_customers_in_the_queue/lambda
  
  X <- data.frame(Utilization, Average_num_of_customers_in_the_system, Average_num_of_customers_in_the_queue, Average_time_a_customer_spends_in_the_system, Average_time_a_customer_spends_in_the_queue)
  
  names(X)<-c('Utilization','Average_num_of_customers_in_the_system','Average_num_of_customers_in_the_queue','Average_time_a_customer_spends_in_the_system','Average_time_a_customer_spends_in_the_queue')
  return(X)
}
mms_performance(15, 20, 2)




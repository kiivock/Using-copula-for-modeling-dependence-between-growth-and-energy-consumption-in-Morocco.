# Using-copula-for-modeling-dependence-between-growth-and-energy-consumption-in-Morocco.

#Load library
library(MASS)
library("copula")
library("CDVine")


#Detecting Dependence With Kendall Plots:
   
   BiCopKPlot(u1,u2, PLOT=TRUE,main="Prod-PIB")
  
   BiCopKPlot(u1,u3, PLOT=TRUE,main="Prod-Consom")
   
   BiCopKPlot(u3,u2, PLOT=TRUE,main="Consom-PIB")
   
   X_1 <- matrix(c(u1,u2),ncol=2,byrow = T)
   X_2 <- matrix(c(u1,u3),ncol=2,byrow = T)
   X_3 <- matrix(c(u2,u3),ncol=2,byrow = T)
   
gofCopula(tCopula(), X_1, N = 20, method = "SnC",
          simulation = c("pb", "mult"), print.every = 100,
          optim.method = "BFGS", optim.control = list(maxit=20))
   

gofCopula(normalCopula(), X_1, N = 20, method = "SnC",
          simulation = c("pb", "mult"), print.every = 100,
          optim.method = "BFGS", optim.control = list(maxit=20))

gofCopula(claytonCopula(), X_1, N = 20, method = "SnC",
                    simulation = c("pb", "mult"), print.every = 100,
               optim.method = "BFGS", optim.control = list(maxit=20))
gofCopula(tCopula(), X_1, N = 20, method = "SnC",
          simulation = c("pb", "mult"), print.every = 100,
          optim.method = "BFGS", optim.control = list(maxit=20))
   

gofCopula(normalCopula(), X_2, N = 20, method = "SnC",
          simulation = c("pb", "mult"), print.every = 100,
          optim.method = "BFGS", optim.control = list(maxit=20))

gofCopula(claytonCopula(), X_2, N = 20, method = "SnC",
                    simulation = c("pb", "mult"), print.every = 100,
               optim.method = "BFGS", optim.control = list(maxit=20))
               
gofCopula(tCopula(), X_2, N = 20, method = "SnC",
          simulation = c("pb", "mult"), print.every = 100,
          optim.method = "BFGS", optim.control = list(maxit=20))
   

gofCopula(normalCopula(), X_3, N = 20, method = "SnC",
          simulation = c("pb", "mult"), print.every = 100,
          optim.method = "BFGS", optim.control = list(maxit=20))

gofCopula(claytonCopula(), X_3, N = 20, method = "SnC",
                    simulation = c("pb", "mult"), print.every = 100,
               optim.method = "BFGS", optim.control = list(maxit=20))
   

gofCopula(normalCopula(), X_3, N = 20, method = "SnC",
          simulation = c("pb", "mult"), print.every = 100,
          optim.method = "BFGS", optim.control = list(maxit=20))

 
#Sequential Estimation of D-vine copula model
  
BiCopSelect(u1, u3, familyset=NA, selectioncrit="AIC",indeptest=FALSE, level=0.05) 

BiCopSelect(u1, u2, familyset=NA, selectioncrit="AIC",indeptest=FALSE, level=0.05) 
   
BiCopSelect(u3, u2, familyset=NA, selectioncrit="AIC",indeptest=FALSE, level=0.05)   
 

Dat<-as.matrix(dat)

d<-dim(Dat)[2]

family<-rep(2,d(d-1)/2)

CDVineSeqEst(Data,family,type=2,method = "mle",max.df=300)

seqPar <- CDVineSeqEst(dat, family = family, type = 1, method = "mle")

mlePar <- CDVineMLE(dat, family = family, start = seqPar$par,start2 = seqPar$par2, type = 1)

CDVineTreePlot(data = NULL, family = family, par = mlePar$par,par2 = mlePar$par2, names = colnames(dat), type = 1, tree = 1,edge.labels = c("family", "theotau"))

CDVineVuongTest(dat, Model1.order = order, Model2.order = order_dvine, Model1.family = family, Model2.family = family_dvine, Model1.par = mlePar$par,Model2.par = par_dvine, Model1.par2 = mlePar$par2,Model2.par2 = par2_dvine, Model1.type = 1, Model2.type = 2)





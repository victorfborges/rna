rm(list=ls())
library('corpcor')# biblioteca para uso da pseudoinversa 
library('RSNNS') #Fornece a rotina para validação 

source('C:/Users/victo/Documents/Workspaces/ws-Rstudio/exercicioRNA-6/ELM.R')

#acesso ao banco de dados desejado
dados <- read.csv2("C:/Users/victo/Documents/UFMG/Livros e Materias/Redes Neurais Artificiais/Exercícios/trabalho-final/occupancy_data/datatest2.txt",
                   header = TRUE, sep = ",", stringsAsFactors =  FALSE)



X <- as.matrix(cbind(as.numeric(dados[,2]), as.numeric(dados[,3]), as.numeric(dados[,4]), as.numeric(dados[,5]), as.numeric(dados[,6])))

Y <- as.matrix(as.numeric(dados[,7]))

for (i in seq(1, nrow(Y), 1)) {
  if(Y[i] == 0){
    Y[i] <- -1
  }
}

XY <- splitForTrainingAndTest(X, Y, ratio = 0.3) #separa 30% para teste
#acesso aos valores de teste e treinamento
Xtrain <- XY$inputsTrain
Ytrain <- XY$targetsTrain
Xtest <- XY$inputsTest
Ytest <- XY$targetsTest

for (i in seq(1, 3, 1)) {
  
  num_neuronios <- 5*i
  retlist <- trainELM(Xtrain, Ytrain, num_neuronios, 1)
  W <- retlist[[1]]
  H <- retlist[[2]]
  Z <- retlist[[3]]
  Yhat <- sign(YELM(Xtest, Z, W, 1))
  error <- sum((Ytest - Yhat)^2)/(length(Yhat)*4)
  cat("hiperparametro: ", num_neuronios, "; erro: ", error, "\n")
}
rm(list=ls())
library('corpcor')# biblioteca para uso da pseudoinversa 
library('RSNNS') #Fornece a rotina para validação 

source('C:/Users/victo/Documents/Workspaces/ws-Rstudio/exercicioRNA-6/ELM.R')

#acesso ao banco de dados desejado
dados <- read.csv2("C:/Users/victo/Documents/UFMG/Livros e Materias/Redes Neurais Artificiais/Exercícios/trabalho-final/occupancy_data/datatest2.txt",
                   header = TRUE, sep = ",", stringsAsFactors =  FALSE)



X <- as.matrix(cbind(as.numeric(dados[,2]), as.numeric(dados[,3]), as.numeric(dados[,4]), as.numeric(dados[,5]), as.numeric(dados[,6])))

Y <- as.matrix(as.numeric(dados[,7]))

XY <- splitForTrainingAndTest(X, Y, ratio = 0.3) #separa 30% para teste
#acesso aos valores de teste e treinamento
Xtrain <- XY$inputsTrain
Ytrain <- XY$targetsTrain
Xtest <- XY$inputsTest
Ytest <- XY$targetsTest

Xtrainext <- cbind(1, Xtrain)
Xtestext <- cbind(1, Xtest)

for (i in seq(1, 3, 1)) {
  
  num_neuronios <- 5*i
  rede <- mlp(Xtrainext , Ytrain, size = num_neuronios, maxit = 500, initFunc = "Randomize_Weights",
              initFuncParams = c(-0.3, 0.3), learnFunc = "Rprop",
              learnFuncParams = c(0.1, 0.1), uptadeFunc = "Topological_Order",
              updateFuncParams = c(0), hiddenActFunc = "Act_Logistic",
              shufflePatterns = TRUE, linOut = FALSE)
  
  Yhat <- predict(rede, Xtestext)
  error <- sum((Ytest - Yhat)^2)/(nrow(Yhat))
  print(cbind(nrow(Yhat), length(Yhat)))
  print(error)
}
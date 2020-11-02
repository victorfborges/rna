rm(list=ls())
library('corpcor')# biblioteca para uso da pseudoinversa 
library('RSNNS') #Fornece a rotina para validação 

#importa as funcoes de treinamento RBF
source('C:/Users/victo/Documents/Workspaces/ws-Rstudio/exercicioRNA-7/RBF.R')

#acesso ao banco de dados desejado
dados <- read.csv2("C:/Users/victo/Documents/UFMG/Livros e Materias/Redes Neurais Artificiais/Exercícios/trabalho-final/wine_quality/winequality-red.csv",
                   header = TRUE, sep = ";", stringsAsFactors =  FALSE, dec = ".")



X <- as.matrix(cbind( as.numeric(dados[,1]),as.numeric(dados[,2]), as.numeric(dados[,3]), as.numeric(dados[,4]), as.numeric(dados[,5]), as.numeric(dados[,6]),
                      as.numeric(dados[,7]), as.numeric(dados[,8]), as.numeric(dados[,9]), as.numeric(dados[,10]), as.numeric(dados[,11])))

Y <- as.matrix(as.numeric(dados[,12]))


Xext <- cbind(1, X)

XY <- splitForTrainingAndTest(Xext, Y, ratio = 0.3) #separa 30% para teste
#acesso aos valores de teste e treinamento
Xtrain <- XY$inputsTrain
Ytrain <- XY$targetsTrain
Xtest <- XY$inputsTest
Ytest <- XY$targetsTest

for (i in seq(1, 3, 1)) {
  num_neuronios <- 5*i
  pRBF <- treinaRBF(Xtrain,Ytrain, num_neuronios) #treinamento da rede
  Yhat<- YRBF(Xtest, pRBF) #saida para entrada de teste
  erro <- sum((Ytest - Yhat)^2)/(length(Yhat)) #calculo do erro medio quadratico
  print(erro)
}
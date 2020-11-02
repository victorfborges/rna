rm(list=ls())
library('corpcor')# biblioteca para uso da pseudoinversa 
library('RSNNS') #Fornece a rotina para validação 

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

Xext <- cbind(1, X)

XY <- splitForTrainingAndTest(Xext, Y, ratio = 0.3) #separa 30% para teste
#acesso aos valores de teste e treinamento
Xtrain <- XY$inputsTrain
Ytrain <- XY$targetsTrain
Xtest <- XY$inputsTest
Ytest <- XY$targetsTest

#calculo do vetor de pesos
W <- pseudoinverse(Xtrain) %*% Ytrain

yhat <- Xtest %*% W

yhat <- sign(yhat)
#for (i in seq(1, nrow(yhat), 1)) {
#  if(yhat[i,] < 0.0000){
#    yhat[i,] <- 0
#  }
#  else{
#    yhat[i,] <- 1
#  }
#}

erro <- sum((yhat - Ytest)^2)/(4*nrow(yhat)) 
print(erro)
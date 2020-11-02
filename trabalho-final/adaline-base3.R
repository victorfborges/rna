rm(list=ls())
library('corpcor')# biblioteca para uso da pseudoinversa 
library('RSNNS') #Fornece a rotina para validação 

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

#calculo do vetor de pesos
W <- pseudoinverse(Xtrain) %*% Ytrain

yhat <- Xtest %*% W

erro <- sum((yhat - Ytest)^2)/(nrow(yhat)) 
print(erro)
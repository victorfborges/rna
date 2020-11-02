rm(list=ls())
library('corpcor')# biblioteca para uso da pseudoinversa 
library('RSNNS') #Fornece a rotina para validação 

source('C:/Users/victo/Documents/Workspaces/ws-Rstudio/exercicioRNA-4/trainperceptron.R')
source('C:/Users/victo/Documents/Workspaces/ws-Rstudio/exercicioRNA-4/yperceptron.R')

#acesso ao banco de dados desejado
dados <- read.csv2("C:/Users/victo/Documents/UFMG/Livros e Materias/Redes Neurais Artificiais/Exercícios/trabalho-final/occupancy_data/datatest2.txt",
                   header = TRUE, sep = ",", stringsAsFactors =  FALSE)

X <- as.matrix(cbind(as.numeric(dados[,2]), as.numeric(dados[,3]), as.numeric(dados[,4]), as.numeric(dados[,5]), as.numeric(dados[,6])))
Y <- as.matrix(as.numeric(dados[,7]))

Xext <- cbind(1, X)

XY <- splitForTrainingAndTest(Xext, Y, ratio = 0.3) #separa 30% para teste
#acesso aos valores de teste e treinamento
Xtrain <- XY$inputsTrain
Ytrain <- XY$targetsTrain
Xtest <- XY$inputsTest
Ytest <- XY$targetsTest

retlist <- trainperceptron(Xtrain, Ytrain, 0.1, 0.0001, 500, 0)
wt <- retlist[[1]] #pega o vetor de trinamento w

Yhat <- yperceptron(Xtest, wt, 0)

erro <- sum((Yhat - Ytest)^2)/(nrow(Yhat)) 
print(erro)

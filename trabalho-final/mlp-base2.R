rm(list=ls())
library('corpcor')# biblioteca para uso da pseudoinversa 
library('RSNNS') #Fornece a rotina para validação 

source('C:/Users/victo/Documents/Workspaces/ws-Rstudio/exercicioRNA-6/ELM.R')


#acesso ao banco de dados desejado
dados <- read.csv2("C:/Users/victo/Documents/UFMG/Livros e Materias/Redes Neurais Artificiais/Exercícios/trabalho-final/dow_jones_index/dow_jones_index.data",
                   header = TRUE, sep = ",", stringsAsFactors =  FALSE)


for (i in seq(1, nrow(dados), 1)) {
  if(dados[i,10] == ''){
    dados[i,10] <- 0
  }
}

X <- as.matrix(cbind(as.numeric(dados[,9]), as.numeric(dados[,10]), as.numeric(dados[,15]), as.numeric(dados[,16])))
Y <- as.matrix(as.numeric(dados[,14]))

X1 <- matrix(0, nrow = (nrow(X)/2), ncol = 4)
X2 <- matrix(0, nrow = (nrow(X)/2), ncol = 4)
Y1 <- matrix(0, nrow = (nrow(X)/2), ncol = 1)
Y2 <- matrix(0, nrow = (nrow(X)/2), ncol = 1)

for (i in seq(1, nrow(X), 1)) {
  
  if(dados[i, 1] == 1){
    X1 <- as.matrix(rbind(X1, X[i,]))
    Y1 <- as.matrix(rbind(Y1, Y[i,]))
  } 
  if(dados[i, 1] == 2){
    X2 <- as.matrix(rbind(X2, X[i,]))
    Y2 <- as.matrix(rbind(Y2, Y[i,]))
  }  
}

for (i in seq(1, nrow(Y2), 1)) {
  if(Y2[i] < 0.0000){
    Y2[i] <- 0
  }
  else{
    Y2[i] <- 1
  }
}
for (i in seq(1, nrow(Y1), 1)) {
  if(Y1[i,] < 0.0000){
    Y1[i,] <- 0
  }
  else{
    Y1[i,] <- 1
  }
}
X1ext <- cbind(1, X1)
X2ext <- cbind(1, X2)

for (i in seq(1, 3, 1)) {
  
  num_neuronios <- 5*i
  rede <- mlp(X1ext, Y1, size = num_neuronios, maxit = 500, initFunc = "Randomize_Weights",
              initFuncParams = c(-0.3, 0.3), learnFunc = "Rprop",
              learnFuncParams = c(0.1, 0.1), uptadeFunc = "Topological_Order",
              updateFuncParams = c(0), hiddenActFunc = "Act_Logistic",
              shufflePatterns = TRUE, linOut = FALSE)
  
  Yhat <- predict(rede, X2ext)
  error <- sum((Y2 - Yhat)^2)/(length(Yhat))
  print(error)
}


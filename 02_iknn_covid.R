# Load packages ---------------------------
library(caret)
library(kohonen)
library(class)
library(cvTools)
library(e1071)
library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)
library(randomForest)
library(scales)
source("iknn.R")


setwd("C:\\Users\\vinicius.grande\\Desktop\\TCC2")

som_dataset <- read.csv("SomCompletoDadosPurocNeuronio.csv")
min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
}
# som_dataset[,2:15] <- (som_dataset[,2:15]/max(som_dataset[,2:15]))
som_dataset$Neuronio <- indice_neuronio
# GERACAO DO DATASET

# LIMITANDO DATASET 
som_dataset <- som_dataset[indice_neuronio_sobreposto, ]
som_dataset[som_dataset$SARS_CoV_2_PCR == 0, 1] <- 2
som_dataset[,2:14] <- scale(som_dataset[,2:14])
# som_dataset[,2:14] <- sapply(som_dataset[,2:14], min_max_norm)

#x<-ledataset[,2:length(ledataset)]

#x = data.frame(x)
#y <- ledataset[,1]
#y <- as.numeric(y)
#dataset <- cbind(x,y)
#
'
##############################################################################
### EXPERIMENTOS COMPARATIVOS PARA ANALISES DE DESEMPENHO 
##############################################################################

c
'
#Fisher s discriminant radio(F1)
# fisherratio <- function(class1,class2){
#   return ((mean(class1) - mean(class2))^2)/(var(class1)+var(class2))}
# #Volume of overlap region(F2)
# F2 <- function(class1,class2){
#   minmax = min(max(class1), max(class2))
#   maxmin = max(min(class1), min(class2))
#   maxmax = max(max(class1), max(class2))
#   minmin = min(min(class1), min(class2))
#   return (minmax - maxmin)/(maxmax-minmin)
# }


#---------------------------------------------------------



#---------------------------------------------------------




# #KFOLD AND IkNN
# library(cvTools)
# N<-dim(datanorm)[1]
# pastas<-5
# folds <- cvFolds(N,K=pastas,type=c("random"))
# acuraciaacerto_knn <- rep(0,pastas)
# 
#npastas <- length(y)/length(unique(y))
#nclasse <- length(unique(y))
pastas <- 5
set.seed(0)
folds <- cvFolds(nrow(som_dataset), K=pastas, type = c("random"))

f1iknn <- NULL
mediaiknn <- NULL
sdiknn <- NULL
mediaf1iknn <- NULL
listaelk <- NULL
listaelki <- NULL
colnameiknn <- NULL
testeacuracia <- NULL


# ABORDAGEM 1 -------------------------------------------------------------

CalcularAbordagem1 <- function(){
    # ESSA ABORDAGEM É NÃO SUPERVISIONADAAAAAA
    for (pastinha in 1:pastas){
        i_treino <- folds$subsets[which(folds$which != pastinha)]
        i_teste <- folds$subsets[which(folds$which == pastinha)]
        #TREINO
        x_treino <- som_dataset[i_treino, 2:length(som_dataset)]
        y_treino <- som_dataset[i_treino, 1]
        #TESTE
        x_teste <- som_dataset[i_teste, 2:length(som_dataset)]
        y_teste <- som_dataset[i_teste, 1]
        
        x_treino_neuronio <- rbind(x_treino,x_teste)
        y_treino_neuronio <- c(y_treino, y_teste)
        unico_neuronio <- unique(x_treino$Neuronio)
        for(num_neuronio in unico_neuronio){
            # NÃO É PRA DIVIDIR EM TREINO/TESTE
            x_treino_inn <- x_treino_neuronio[which(x_treino_neuronio$Neuronio == num_neuronio), -14]
            y_treino_inn <- y_treino_neuronio[which(x_treino_neuronio$Neuronio == num_neuronio)]
            vetor_peso_teste <- vetor_peso_neuronio[num_neuronio,]
            iknn_som_neuronio <- as.factor(iknn(as.matrix(x_treino_inn),
                                                as.matrix(y_treino_inn),
                                                as.matrix(vetor_peso_teste), 0,
                                                length(vetor_peso_teste) * 0.25, 1))
            
            confusion_matrix_ab1 <- table(vetor_peso_teste, iknn_som_neuronio)
            acuracia_ab1 <- sum(diag(confusion_matrix_ab1))/sum(confusion_matrix_ab1)
        }
    }
}
#descomenta
# #priorizar abordagem 1 e abordagem 2 para trabalhos futuros

# ABORDAGEM 2 -------------------------------------------------------------
CalcularAbordagem2 <- function(){
    
    juizo_inicial <- Sys.time()
    #for(elk in  seq(1,15,2)) {
    #  for(elki in 1:elk){
    acuracia_iknn <- NULL
    sd_iknn <- NULL
    acuracia_sem_iknn <- NULL
    tempo_iknn <- NULL
    tempo_sem_iknn <- NULL
    for (pastinha in 1:pastas){
        i_treino <- folds$subsets[which(folds$which != pastinha)]
        i_teste <- folds$subsets[which(folds$which == pastinha)]
        #treino
        x_treino <- som_dataset[i_treino,2:length(som_dataset)]
        x_treino_mat <- as.matrix(x_treino)
        y_treino <- som_dataset[i_treino,1]
        y_treino_mat <- as.matrix(y_treino)
        x_teste<-som_dataset[i_teste, 2:(length(som_dataset))]
        x_teste_mat <- as.matrix(x_teste)
        y_teste<-som_dataset[i_teste, 1]
        y_teste_mat <- as.matrix(y_teste)
        #rodar o iknn e o knn sem o som(medir o tempo do algoritmo) 
        # y_teste_sem_som <- y_teste
        start.time <- Sys.time()
        sem_som_iknn <- as.factor(iknn(x_treino_mat, y_treino_mat, x_teste_mat, y_teste_mat, 15, 15))
        end.time <- Sys.time()
        tempo_sem_som_iknn <- as.numeric(difftime(end.time, start.time), units = "secs")
        # y_teste_sem_som <- as.factor(y_teste_sem_som)
        y_teste_mat <- as.factor(y_teste_mat)
        confusion_matrix_sem <- table(y_teste_mat, sem_som_iknn)
        acuracia_sem_som_iknn <- sum(diag(confusion_matrix_sem))/sum(confusion_matrix_sem)
        ##gerando som com conjunto de treino
        
        
        # observando a estrutura e dimensão da base de dados covid str(som_df)
        # dim(som_treino_dataset)
        # preparando os dados para o som
        # treino_som_mat <- as.matrix(scale(x_treino[-14]))
       
        # criando o grid que será usado no som
        covid_treino_grid <- kohonen::somgrid(xdim = 4, ydim = 4, topo = "hexagonal")
        # setando uma seed
        set.seed(1)
        # usando a função som
        # covid_treino_som_model <- som(x = treino_som_mat, 
        #                               grid = covid_treino_grid)
        covid_treino_som_model <- kohonen::som(as.matrix(som_dataset[,2:14]), 
                                      grid = covid_treino_grid)
        vetor_peso <- covid_treino_som_model$codes[[1]]
        
        x_treino$Neuronio <- covid_treino_som_model$unit.classif[i_treino]
        x_treino_mat <- as.matrix(x_treino)
        
        y_treino <- som_dataset[i_treino,1]
        y_treino_mat <- as.matrix(y_treino)
        
        #TESTE
        x_teste<-som_dataset[i_teste, 2:(length(som_dataset))]
        x_teste$Neuronio <- covid_treino_som_model$unit.classif[i_teste]
        x_teste_mat <- as.matrix(x_teste)
        y_teste<-som_dataset[i_teste, 1]
        y_teste_mat <- as.matrix(y_teste)
        tamanho_lista <-  dim(x_teste)[1]
        vet_acuracia <- vector(mode = "double", length = tamanho_lista)
        vet_tempo <- vector(mode = "double", length = tamanho_lista)
        matrizfor <- as.matrix(x_teste[,-14])
        vetorbindado <- rbind(matrizfor, vetor_peso)
       fim_vetor <- nrow(vetorbindado)
       inicio_vetor <- fim_vetor - 15
        #MATRIZFOR2 <- X_TREINO
        #MATRIZFOR <-X_TESTE
        # for (ind_x_teste in 1:dim(x_teste)[1]){
        
        # for (ind_x_teste in 1:dim(matrizfor)[1]){
            # dist_df <- as.matrix(dist(vetorbindado[c(ind_x_teste, inicio_vetor:fim_vetor), ], 
            #                             method = "euclidean"))
            
            x_treino_n <- x_treino_mat[,14]
            x_teste_n <- x_teste_mat[,14]
         for (ind_x_teste in 1:nrow(matrizfor)){
            # dist_df<- as.matrix(dist(rbind(x_teste[ind_x_teste, -14], vetor_peso),
            #                                 method = "euclidean"))
            # dist_df <- as.matrix(dist(rbind(matrizfor[ind_x_teste,], vetor_peso), 
            #                           method = "euclidean"))
              # DESCOMENTA SAPORRA
             
             
             dist_df <- as.matrix(dist(vetorbindado[c(ind_x_teste, inicio_vetor:fim_vetor), ],
                                        method = "euclidean"))
             
             
            sort_neuronio <- sort(dist_df[-1,1])[1]
            min_neuronio <- match(sort_neuronio, dist_df[-1,1])
            # min_neuronio <- (which(min(dist_df[-1,1]) == dist_df)[1] - 1)
            print(min_neuronio)
            #DESCOMENTA SAPORRA 
             
            # sort_neuronios <- sort(dist_df[-1,1])[1]
            # min_neuronios <- match(sort_neuronios, dist_df[-1,1])
            
            
            # x_treino_neuronio <- x_treino_mat[which(x_treino_mat[,14] == min_neuronio), -14]
            #  print(dim(x_treino_neuronio))
            # x_teste_neuronio <- x_teste_mat[which(x_teste_mat[,14] == min_neuronio), -14]
            #  print(dim(x_teste_neuronio))
            # y_treino_neuronio <- y_treino_mat[which(x_treino_mat[,14] == min_neuronio)]
            #  print(dim(y_treino_neuronio))
            # y_teste_neuronio <- y_teste_mat[which(x_teste_mat[,14] == min_neuronio)]
            #  print(dim(y_teste_neuronio))
           
             
             
            # x_treino_neuronios <- x_treino_mat[which(x_treino_mat[,14] %in% min_neuronios), -14]
            # x_teste_neuronios <- x_teste_mat[which(x_teste_mat[,14] %in% min_neuronios), -14]
            # y_treino_neuronios <- y_treino_mat[which(x_treino_mat[,14] %in% min_neuronios)]
            # y_teste_neuronios <- y_teste_mat[which(x_teste_mat[,14] %in% min_neuronios)]
            
            
            x_treino_n <- unname(x_treino_n)
            x_treino_neuronios <- x_treino_mat[1665, -14]
            print(length(x_treino_neuronios))
            x_teste_neuronios <- x_teste_mat[x_teste_n == min_neuronio, -14]
            print("X_TESTE_NEURONIO")
            print(dim(x_teste_neuronios))
            y_treino_neuronios <- y_treino_mat[x_treino_n %in% min_neuronio]
            print("Y_TREINO_NEURONIO")
            print(dim(y_treino_neuronios))
            y_teste_neuronios <- y_teste_mat[x_teste_n %in% min_neuronio]
            print("Y_TESTE_NEURONIO")
            print(dim(y_teste_neuronios))
            start.time <- Sys.time()
            som_iknn <-as.factor(iknn(as.matrix(x_treino_neuronios),
                             as.matrix(y_treino_neuronios),
                             as.matrix(x_teste_neuronios),
                             as.matrix(y_teste_neuronios),
                             # length(y_teste_neuronios) * 0.25, 1))
                             1,1))
            # som_iknn <- rep(1, length(y_teste_neuronios))
            #rodar com nrow x_treino_neuronios no iknn para valor de k
            #trocar valor do i por 3
            #trocar dimensão do mapa para 5x5
            
            end.time <- Sys.time()
            tempo_som_iknn <- as.numeric(difftime(end.time, start.time), units = "secs")
            vet_tempo[ind_x_teste] <- tempo_som_iknn
            y_teste_neuronios <- as.factor(y_teste_neuronios)
            confusion_matrix <- table(y_teste_neuronios, som_iknn)
            acuracia_som_iknn <- sum(diag(confusion_matrix))/sum(confusion_matrix)
            vet_acuracia[ind_x_teste] <- acuracia_som_iknn 
        } 
        assign("vet_acuraciarapido", vet_acuracia, envir = globalenv())
        tempo_iknn <- c(tempo_iknn, mean(vet_tempo))
        tempo_sem_iknn <- c(tempo_sem_iknn, tempo_sem_som_iknn)
        acuracia_iknn <- c(acuracia_iknn, mean(vet_acuracia))
        sd_iknn <- c(sd_iknn, sd(vet_acuracia))
        acuracia_sem_iknn <- c(acuracia_sem_iknn, acuracia_sem_som_iknn)
        assign("acuracia_sem_iknnrapido", acuracia_sem_iknn, envir = globalenv())
        assign("acuracia_iknnrapido", acuracia_iknn, envir = globalenv())
    }    
        assign("xtreinoiknnrapido", x_treino_neuronios, envir=globalenv())
        assign("xtesteiknnrapido", x_teste_neuronios, envir=globalenv())
        assign("ytreinoiknnrapido", y_treino_neuronios, envir=globalenv())
        assign("ytesteiknnrapido", y_teste_neuronios, envir=globalenv())
    juizofinal <- Sys.time( )
    matrix_resultados <- matrix(data = c(acuracia_iknn, sd_iknn, tempo_iknn), nrow= 3, ncol = 5, byrow = TRUE)
    colnames(matrix_resultados) <- c("Pasta 1", "Pasta 2", "Pasta 3", "Pasta 4", "Pasta 5")
    row.names(matrix_resultados) <- c("Acurácia(Média)", "Acurácia(Desvio padrão)", "Tempo(Modelo)")
    matrix_resultados <- round(matrix_resultados, digits = 3)
    assign("matrix_resultadosrapido", matrix_resultados, envir = globalenv())
    tempototal <- as.numeric(difftime(juizofinal, juizo_inicial), units = "secs")
    print_tempo_total <- sprintf("O tempo total da Abordagem 2 foi de %.2f segundos", tempototal)
    print(print_tempo_total)
    assign("tempototal", tempototal, envir = globalenv())
    print_acuracia_iknn <- sprintf("A acuracia média do IKNN na abordagem 2 com o SOM foi de %.3f", mean(acuracia_iknn))
    print(print_acuracia_iknn)
    assign("acuracia_abordagem2", mean(acuracia_iknn), envir = globalenv())
    print_tempo_medio <- sprintf("O tempo médio  do IKNN na abordagem 2  com o SOM foi de %.2f segundos", mean(tempo_iknn))
    print(print_tempo_medio)
    print_acuracia_sem_iknn <- sprintf("A acuracia média do IKNN na abordagem 2 sem o SOM foi de %.3f", mean(acuracia_sem_iknn))
    print(print_acuracia_sem_iknn)
    print_tempo_medio_sem <- sprintf("O tempo médio  do IKNN na abordagem 2  sem o SOM foi de %.2f segundos", mean(tempo_sem_iknn))
    print(print_tempo_medio_sem)
    assign("tempo_abordagem2", mean(tempo_iknn), envir = globalenv())
}   

#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     cmcaret <- confusionMatrix(modelbestiknn, Y_teste)
#     cmcaret$byClass[is.na(cmcaret$byClass)] <- 0
# 
#     assign("modelbestiknn", modelbestiknn, envir = globalenv())
#     assign("cm", cm,envir = globalenv())
#     # if(mean(modelbestiknn == 0)){
#     #   acuracia <- cm[1,1]/sum(cm)
#     #   assign("acuracia",acuracia,envir = globalenv())
#     #   cat("Caso1")
#     # }else if(mean(modelbestiknn == 1)){
#     #   acuracia <- cm[2,1]/sum(cm)
#     #   assign("acuracia",acuracia,envir = globalenv())
#     #   cat("Caso2")
#     # } else{
#     acuracia <- sum(diag(cm))/sum(cm)
#     #cat("Caso3?")
#     assign("acuracia", acuracia, envir = globalenv())
#     #}
#     precision <- cm[1,1]/sum(cm[1, 1:2])
#     recall <- cm[1,1]/sum(cm[1:2, 1])
#     if(nclasse == 2){
#       f1 <- cmcaret$byClass[7]
#     }else{
#       f1 <- cmcaret$byClass[,7]
#     }
#     acuraciaiknn <- c(acuraciaiknn,acuracia)
#    f1iknn <- c(f1iknn,f1)
#     #colnameiknn <- c(colnameiknn,paste(toString(el),toString(elki),sep = ""))
#     #cmcaret<- confusionMatrix(modelbestiknn,as.factor(Y_teste))
#     assign("acuraciaiknn",acuraciaiknn,envir = globalenv())
#     assign("f1iknn", f1iknn, envir = globalenv())
#     cmcaret$byClass[is.na(cmcaret$byClass)] <- 0
#   }
#     sdiknn <- c(sdiknn,sd(acuraciaiknn))
#     mediaiknn <- c(mediaiknn,(mean(acuraciaiknn)))
#     mediaf1iknn <- c(mediaf1iknn,mean(f1iknn))
#     assign("mediaiknn", mediaiknn, envir = globalenv())
#     assign("sdiknn",sdiknn,envir = globalenv())
#     assign("mediaf1iknn", mediaf1iknn, envir = globalenv())
# #    listaelk <- c(listaelk,elk)
# #    listaelki <- c(listaelki,elki)
# #    assign("listaelk", listaelk, envir = globalenv())
# #    assign("listaelki", listaelki, envir = globalenv())
#     cmcaret$byClass[is.na(cmcaret$byClass)] <- 0
#   
# #}
# 
#   
# #}
# 
# cmcaret$byClass[is.na(cmcaret$byClass)] <- 0
# listaelk <- listaelk[1:64]
# listaelki <- listaelki[1:64]
# 
# miknn <- matrix(mediaiknn,1,ncol = 64, byrow = TRUE)
# #colnames(miknn) <- paste(listaelk,listaelki)
# row.names(miknn) <- c(1:1)
# mf1iknn <- matrix(mediaf1iknn,nrow = 1,ncol = 64, byrow = TRUE)
# #colnames(mf1iknn) <- paste(listaelk,listaelki)
# row.names(mf1iknn) <- c(1:1)
# msdiknn <- matrix(sdiknn,1,ncol = 64, byrow = TRUE)
# #colnames(msdiknn) <- paste(listaelk,listaelki)
# row.names(miknn) <- c(1:1)
# 
# 
# max(colMeans(miknn))
# max(colMeans(msdiknn))
# max(colMeans(mf1iknn))
# 
# 

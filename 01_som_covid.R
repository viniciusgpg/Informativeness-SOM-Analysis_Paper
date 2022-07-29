# Load packages ---------------------------

library(kohonen)
library(aweSOM)
library(cluster)

setwd("C:\\Users\\vinicius.grande\\Desktop\\TCC2")
som_dataset <- read.csv("SomCompletoDadosPurocNeuronio.csv")

# Observando a estrutura e dimensão da base de dados Covid 

str(som_dataset)
som_dataset <- som_dataset[, -15] 
dim(som_dataset)

# Preparando os dados para o SOM
min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
}

som_dataset[,2:14] <- scale(som_dataset[,2:14])
som_dataset_mat <- as.matrix(scale(som_dataset[, 2:14]))
# som_dataset_mat <- as.matrix(som_dataset[, 2:14])

# Criando o grid que será usado no SOM

som_grid <- kohonen::somgrid(xdim = 5, ydim = 5, topo = "hexagonal")

# Setando uma seed

set.seed(0)
# Usando a função SOM

som_model <- kohonen::som(X = som_dataset_mat,  grid = som_grid)

plot(som_model)

#Criando as classes do aweSOM

superclust_pam <- cluster::pam(som_model$codes[[1]], 1)
superclasses_pam <- superclust_pam$clustering

#Plotando informações gerais do mapa

aweSOMplot(som = som_model, type = "Cloud", data =  som_dataset,
           variables = c("SARS_CoV_2_PCR", "Hematocrit", "Hemoglobin",
                         "Platelets", "Red.blood.Cells", "Lymphocytes.Absoluto",
                         "Mean.corpuscular.hemoglobin.concentrationá.MCHC.",
                         "Leukocytes", "Basophils.Absoluto",
                         "Mean.corpuscular.hemoglobin..MCH.",
                         "Eosinophils.Absoluto",
                         "Mean.corpuscular.volume..MCV.",
                         "Monocytes.Absoluto",
                         "Red.blood.cell.distribution.width..RDW."),
           superclass = superclasses_pam)

aweSOMplot(som = som_model, type = "Hitmap", data =  
           som_dataset, variables = 
                c("Hematocrit" ,"Hemoglobin", "Platelets","Red.blood.Cells",
                  "Lymphocytes.Absoluto",
                  "Mean.corpuscular.hemoglobin.concentrationá.MCHC.",
                  "Leukocytes", "Basophils.Absoluto",
                  "Mean.corpuscular.hemoglobin..MCH.",
                  "Eosinophils.Absoluto",
                  "Mean.corpuscular.volume..MCV.",
                  "Monocytes.Absoluto",
                  "Red.blood.cell.distribution.width..RDW."),
           superclass = superclasses_pam)

UMatrix <- aweSOMplot(som = som_model, type = "UMatrix", 
                      data =  som_dataset, 
                      variables = 
                          c("Hematocrit", "Hemoglobin" ,"Platelets",
                            "Red.blood.Cells", "Lymphocytes.Absoluto",
                            "Mean.corpuscular.hemoglobin.concentrationá.MCHC.",
                            "Leukocytes" ,"Basophils.Absoluto",
                            "Mean.corpuscular.hemoglobin..MCH.",
                            "Eosinophils.Absoluto",
                            "Mean.corpuscular.volume..MCV.",
                            "Monocytes.Absoluto",
                            "Red.blood.cell.distribution.width..RDW." ),
                      superclass = superclasses_pam)

# Plotando os resultados
aweSOMplot(som = som_model, type = "Circular", data =  som_dataset,
           variables = c("Hematocrit", "Hemoglobin", "Platelets",
                         "Red.blood.Cells" ,"Lymphocytes.Absoluto",
                         "Mean.corpuscular.hemoglobin.concentrationá.MCHC.",
                         "Leukocytes", "Basophils.Absoluto",
                         "Mean.corpuscular.hemoglobin..MCH.",
                         "Eosinophils.Absoluto",
                         "Mean.corpuscular.volume..MCV.", 
                         "Monocytes.Absoluto",
                         "Red.blood.cell.distribution.width..RDW." ),
           superclass = superclasses_pam)

aweSOMplot(som = som_model, type = "Barplot", data =  som_dataset,
           variables = c("Hematocrit" ,"Hemoglobin" ,"Platelets",
                         "Red.blood.Cells" ,"Lymphocytes.Absoluto",
                         "Mean.corpuscular.hemoglobin.concentrationá.MCHC.",
                         "Leukocytes", "Basophils.Absoluto",
                         "Mean.corpuscular.hemoglobin..MCH.",
                         "Eosinophils.Absoluto",
                         "Mean.corpuscular.volume..MCV.",
                         "Monocytes.Absoluto",
                         "Red.blood.cell.distribution.width..RDW." ),
           superclass = superclasses_pam)

# Plotando variávieis categóricas

aweSOMplot(som = som_model, type = "CatBarplot", data =  som_dataset,
           variables = "SARS_CoV_2_PCR" , superclass = superclasses_pam)

aweSOMplot(som = som_model, type = "Pie", data =  som_dataset,
           variables = "SARS_CoV_2_PCR" , superclass = superclasses_pam)


indice_neuronio <- som_model$unit.classif
vetor_peso_neuronio <- som_model$codes[[1]]
library(ggsom)
covid_som <- kohonen::som(X = as.matrix(som_dataset[2:14]),
                          grid =  kohonen::somgrid(xdim = 5,
                                                   ydim = 5,
                                                   neighbourhood.fct = 
                                                    "gaussian",
                                                   topo = "rectangular"),
                         rlen = 100) 
#topologia hexagonal e aumentar número de interações
plot(covid_som)


# Self Organizing Maps
# Divisão dos dados
set.seed(0)
ind <- sample(2, nrow(som_dataset), replace = TRUE, prob = c(0.7, 0.3))
train <- som_dataset[ind == 1,]
test <- som_dataset[ind == 2,]

# Normalização
train_x <- scale(train[,2:14])
test_x <- scale(test[,2:14],
               center = attr(train_x, "scaled:center"),
               scale = attr(train_x, "scaled:scale"))
train_y <- factor(train[,1])
y <- factor(test[,1])
#test[,1] <- 0
test_xy <- list(independent = test_x, dependent = test[,1])
plot(som_model)
som_model$codes
test_y <- factor(test[,1])

# Modelo de classifica??o e predição

set.seed(42)
#map1 <- xyf(trainX,
#            classvec2classmat(factor(trainY)),
#            grid = somgrid(5, 5, "hexagonal"),
#            rlen = 100)
map1 <- xyf(train_x,
            factor(train_y),
            grid = kohonen::somgrid(5, 5, "hexagonal"),
            rlen = 100)
plot(map1)

# Predição
pred <- predict(map1, newdata = test_xy)
table(Predicted = pred$predictions[[2]], Actual = y)


# Fronteira dos clusters
par(mfrow = c(1,2))
plot(map1, 
     type = 'codes',
     main = c("Codes X", "SOM com a base Covid"))
map1.hc <- cutree(hclust(dist(map1$codes[[2]])), 2)
add.cluster.boundaries(map1, map1.hc)
par(mfrow = c(1,1))
plot(som_model$unit.classif)
plot(som_model, type = "mapping",   labels = som_dataset[,1])
som_model$data

# ind_neuronio_sobreposto <- c(9, 13, 14, 17, 18, 19, 22, 23)
ind_neuronio_sobreposto <- c(2, 8, 13:15, 18:20, 22:25)
neuronios_sobrepostos <- UMatrix$x$cellNames[ind_neuronio_sobreposto]

neuronios_sobrepostos <- gsub(" ", "", neuronios_sobrepostos)
neuronio_split <- strsplit(neuronios_sobrepostos, ",")
indice_neuronio_sobreposto <- as.numeric(unlist(neuronio_split))

                           
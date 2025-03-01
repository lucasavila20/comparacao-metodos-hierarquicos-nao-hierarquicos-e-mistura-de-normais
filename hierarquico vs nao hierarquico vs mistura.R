library(bgmm)
# nao hierarquico
data(genotypes)
dataset <- rbind(genotypes$X, genotypes$knowns)
hist(rbind(dataset[,1], dataset[,2]))
set.seed(1)
modelokmeans <- kmeans(dataset, centers = 3)
previsoes <- modelokmeans$cluster
plot(dataset, col = previsoes)


modelokmeansCENTRADO <- kmeans(dataset, 
                               centers = matrix(genotypes$knowns[c(1,10,15), ],
                                                ncol = 2, byrow= FALSE))

previsoes <- modelokmeansCENTRADO$cluster
plot(dataset, col = previsoes, pch = 19)
points(modelokmeansCENTRADO$centers, cex = 3,
       col = c("blue"), pch = 3, lwd = 3)


#####################################################################
############################HIERARQUICO #############################
hc_average <- hclust(dist(dataset), method = "average")
plot(hc_average, 
     main = "Average",
     xlab = "", sub = "", cex = 0.9)

clusters_average <- cutree(hc_average, k = 3)

# Visualizando os clusters
plot(dataset, col = clusters_average, pch = 19, main = "Average")



hc_single <- hclust(dist(dataset), method = "single")
plot(hc_single, 
     main = "Single",
     xlab = "", sub = "", cex = 0.9)

clusters_single <- cutree(hc_single, k = 3)

# Visualizando os clusters
plot(dataset, col = clusters_single, pch = 19, main = "Single")
######################################################################
######################################################################

modelUnSupervised = unsupervised(X=dataset, k=3)
plot(modelUnSupervised)
grupos <- apply(modelUnSupervised$tij, 1, which.max)

plot(dataset, col = grupos)
points(tail(dataset,15), col = "blue",
       cex = 2, pch = 4, lwd = 3)

contour(
  (MASS::kde2d(dataset[,1], dataset[,2],
               lims = c(0,1,0,1)))
  
)
####################################################################
####################################################################
####################################################################

# BEM SEPARADOS
set.seed(1)
simulados <- c(rnorm(1000,5,1),
               rnorm(500,-5,1), 
               rnorm(500,0,1))

# MISTURA DE NORMAIS

plot(density(simulados, kernel = "gaussian"))
modeloSimulado = unsupervised(X=simulados, k=3)
plot(modeloSimulado)
modeloSimulado$pi
modeloSimulado$mu
modeloSimulado$cvar
cores <- apply(modeloSimulado$tij, 1, which.max)
plot(simulados, col = cores)


# HIERARQUICO

hc_averageSIMU <- hclust(dist(simulados), method = "average")
plot(hc_averageSIMU, 
     main = "Average",
     xlab = "", sub = "", cex = 0.9)

clusters_averageSIMU <- cutree(hc_averageSIMU, k = 3)

# Visualizando os clusters
plot(simulados, col = clusters_averageSIMU, 
     pch = 19, main = "Average")

# NAO HIERARQUICO

modelokmeansSIMU <- kmeans(simulados, 
                           centers = 3)
previsoesSIMU <- modelokmeansSIMU$cluster
plot(simulados, col = previsoesSIMU)
points(modelokmeansSIMU$centers, cex = 3,
       col = c("blue"), pch = 3)



###################################################
# MAL SEPARADOS
#########################################################
#########################################################

simulados <- bgmm::simulateData(d=2, k=3, n=300,
                                m=3,
                                mu=matrix(c(1,1,0,.25,-1,-1), 
                                          ncol = 2, byrow = TRUE),
                                cov = "0",
                                s.pi=c(1/3,1/3,1/3)
)
simulados
plot(simulados$X, pch=19)
plot(simulados$X, pch=19, col = simulados$Ytrue)
#points(simulados$X, col = simulados$Ytrue)
contour(MASS::kde2d((simulados$X)[,1], (simulados$X)[,2]))
#plot(density(simulados))

modelokmeansSIMU <- kmeans(simulados$X, 
                           centers = 3)
previsoesSIMU <- modelokmeansSIMU$cluster
plot(simulados$X, col = previsoesSIMU, pch=19)
points(modelokmeansSIMU$centers, cex = 3,
       col = c("blue"), pch = 3)

mod <- unsupervised(simulados$X, k = 3)
plot(simulados$X, col = apply(mod$tij, 1, which.max), pch=19)

mod$pi
mod$mu
mod$cvar
plot(mod)
hi <- hclust(dist(simulados$X), method = "average")
plot(hi)

ccc <- cutree(hi, 4) 
plot(simulados$X, col = ccc, pch=19)

ccc <- cutree(hi, 3)
plot(simulados$X, col = ccc, pch=19)
plot(mod$X)
head(mod$tij)
abline(v = c(100,200))

table(simulados$Ytrue[4:length(simulados$Ytrue)], 
      ccc)
table(simulados$Ytrue[4:length(simulados$Ytrue)], 
      apply(mod$tij, 1, which.max))
table(simulados$Ytrue[4:length(simulados$Ytrue)], 
      previsoesSIMU)


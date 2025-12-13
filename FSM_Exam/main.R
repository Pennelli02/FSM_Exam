# Inizializzazione
rm(list=ls(all=TRUE))

#Carico dataset
require(gRbase)
data(wine)

#Visualizzazione struttura
str(wine)

#visualizzazioni statistiche del dataset
summary(wine)

#Togliamo dal dataset la variabile categorica Cult
wine_dataset<-subset(wine, select = -Cult)
str(wine_dataset)

# Creiamo tre dataset dei vini raggruppati per tipo di cultura (v1, v2, v3)
list_wine<-split(wine, wine$Cult)

# visualizziamo tutte e tre le liste di vini separati per Cult
str(list_wine) 

# così ne visulizziamo solo una
str(list_wine$v1) 
str(list_wine$v2)
str(list_wine$v3)
#summary(list_wine)

summary(list_wine$v1)
summary(list_wine$v2)
summary(list_wine$v3)

#mi fornisce la statistica colore dell'intera popolazione
summary(wine_dataset$Clri)

#mi fornisce le statistiche del colore per una singola lista di vini 
#v1
summary(list_wine$v1$Clri)
#v2
summary(list_wine$v2$Clri)
#v3
summary(list_wine$v3$Clri)

#Usando R base per la realizzazione grafico

# Calcolo densità di tutti i gruppi
dens_pop <- density(wine_dataset$Clri)
dens_v1  <- density(list_wine$v1$Clri)
dens_v2  <- density(list_wine$v2$Clri)
dens_v3  <- density(list_wine$v3$Clri)

# Determino il massimo valore di y per includere tutte le curve
ymax <- max(dens_pop$y, dens_v1$y, dens_v2$y, dens_v3$y)

plot(dens_pop, main="Density plot di Clri - Popolazione e Cultivar", xlab="Clri", lwd=4, col=rgb(0,0,0,0.7), ylim=c(0,ymax*1.1)) # aggiungo un 10% per margine

# Aggiunta delle altre densità
lines(dens_v1, lwd=2, col=rgb(0,1,0,0.5))
lines(dens_v2, lwd=2, col=rgb(1,0,0,0.5))
lines(dens_v3, lwd=2, col=rgb(0,0,1,0.5))

# Legenda
legend("topright", legend=c("Popolazione","v1","v2","v3"), 
       col=c("black","green","red","blue"), lwd=2)

# install.packages("ggplot2")
#library(ggplot2)

# Boxplot dell'intera popolazione
boxplot(wine_dataset$Clri,
        main = "Boxplot di Clri - Intera popolazione",
        ylab = "Clri",
        col = "lightblue")

# Boxplot suddiviso per cultivar
boxplot(Clri ~ Cult,
        data = wine,
        main = "Boxplot di Clri per Cultivar",
        xlab = "Cultivar",
        ylab = "Clri",
        col = c("lightgreen", "lightpink", "lightblue"))

# Dividere i dataset rimuovendo la variabile categorica Cult per una questione estetica
v1 <- subset(list_wine$v1, select = -Cult)
v2 <- subset(list_wine$v2, select = -Cult)
v3 <- subset(list_wine$v3, select = -Cult)

#Matrici Scatter Plot
#Intera popolazione
plot(wine_dataset, main="Scatterplot Matrix dell'intera popolazione")

#Diviso per tipo di cultivar
plot(v1, main="Scatterplot Matrix di v1")
plot(v2, main="Scatterplot Matrix di v2")
plot(v3, main="Scatterplot Matrix di v3")



# Matrice di correlazione
#intera popolazione
corr <- cor(wine_dataset)

# Matrici di correlazione per ciascuna cultivar
corr1 <- cor(v1)
corr2 <- cor(v2)
corr3 <- cor(v3)

#heatmap
# install.packages("pheatmap")
library(pheatmap)
# Intera popolazione
pheatmap(corr,
         main = "Heatmap delle correlazioni - Popolazione",
         fontsize = 10)

# Per ciascuna cultivar
pheatmap(corr1, main = "Cultivar v1")
pheatmap(corr2, main = "Cultivar v2")
pheatmap(corr3, main = "Cultivar v3")

#-------------------------------------
#install.packages("gRbase")
#install.packages("gRain")
#install.packages("gRim")
# pacchetti per i modelli grafi indiretti
library(gRbase)
library(gRain)
library(gRim)

#Matrici di concentrazione
#intera popolazione
CovPop <- cov.wt(wine_dataset, method = "ML")$cov
concPop <-solve(CovPop)
round(100*concPop)

#diviso per cultivar
#V1
CovV1 <- cov.wt(v1, method = "ML")$cov
concV1 <- solve(CovV1)
round(100*concV1)

#V2
CovV2 <- cov.wt(v2, method = "ML")$cov
concV2 <- solve(CovV2)
round(100*concV2)

#V3
CovV3 <- cov.wt(v3, method = "ML")$cov
concV3 <- solve(CovV3)
round(100*concV3)


#Matrici delle correlazioni parziali

#intera popolazione
popCP <- cov2pcor(concPop)
round(100*popCP)

#diviso per cultivar

#V1
v1CP <- cov2pcor(concV1)
round(100*v1CP)

#V2
v2CP <- cov2pcor(concV2)
round(100*v2CP)

#V3
v3CP <- cov2pcor(concV3)
round(100*v3CP)

#--------------------------------------------------------------
#Grafi indiretti

library(gRbase)
library(gRain)
library(gRim)

#Intera popolazione
#creazione modello saturo 
pop_mod_sat <- cmod(~ .^., data=wine_dataset)

#creazione modello di indipendenza
pop_mod_ind <- cmod(~.^1, data=wine_dataset)

#grafico usando penalizzazione AIC Forward
AIC_pop_F <- stepwise(pop_mod_ind, direction="forward")
plot(AIC_pop_F, "neato")
title(main="UG Forward AIC intera popolazione")

#grafico usando AIC Backward
AIC_pop_B <- stepwise(pop_mod_sat)
plot(AIC_pop_B, "neato")
title(main = "UG Backward AIC intera popolazione")

#grafico usando BIC forward
BIC_pop_F <- stepwise(pop_mod_ind, direction = "forward", 
                      k=log(nrow(wine_dataset)))
plot(BIC_pop_F, "neato")
title(main = "UG Forward BIC intera popolazione")

#grafico usando BIC backward
BIC_pop_B <- stepwise(pop_mod_sat, k=log(nrow(wine_dataset)))
plot(BIC_pop_B)
title(main="UG Backward BIC intera popolazione")

#Both directions
AIC_pop_FB <- stepwise(pop_mod_ind, direction="both")
BIC_pop_FB <- stepwise(pop_mod_ind, direction="both", 
                       k=log(nrow(wine_dataset)))

#grafico usando AIC Both
plot(AIC_pop_FB)
title(main = "UG Both AIC intera popolazione")

#grafico usando BIC Both
plot(BIC_pop_FB)
title(main="UG Both BIC intera popolazione")

#procedura glasso

#pacchetti da installare
#install.packages("glasso")
#install.packages("igraph")
library(glasso)
library(igraph)

# 1. Matrice di correlazione
popCor <- cov2cor(CovPop)

# 2. Graphical Lasso
pop_lasso <- glasso(popCor, rho = 0.3)

# 3. Matrice di adiacenza booleana
AM <- pop_lasso$wi != 0
diag(AM) <- FALSE

# 4. Costruzione del grafo igraph
graf_lasso <- graph_from_adjacency_matrix(
  AM,
  mode = "undirected",
  diag = FALSE
)

# 5. Etichette dei nodi
V(graf_lasso)$name <- colnames(wine_dataset)

plot(main="UG glasso intera popolazione con rho= 0.3", 
  graf_lasso,
  layout = layout_with_kk,
)


#----------------------------------------
#V1
v1_mod_sat <- cmod(~ .^., data=v1)
v1_mod_ind <- cmod(~.^1, data=v1)

#garfico usando penalizzazione AIC Forward
AIC_v1_F <- stepwise(v1_mod_ind, direction="forward")
plot(AIC_v1_F, "neato")
title(main="UG Forward AIC cultivar v1")

#grafico usando AIC Backward
AIC_v1_B <- stepwise(v1_mod_sat)
plot(AIC_v1_B, "neato")
title(main = "UG Backward AIC cultivar v1")

#grafico usando BIC forward
BIC_v1_F <- stepwise(v1_mod_ind, direction = "forward", k=log(nrow(v1)))
plot(BIC_v1_F, "neato")
title(main = "UG Forward BIC cultivar v1")

#grafico usando BIC backward
BIC_v1_B <- stepwise(v1_mod_sat, k=log(nrow(v1)))
plot(BIC_v1_B)
title(main="UG Backward BIC cultivar v1")

#Both directions
AIC_v1_FB <- stepwise(v1_mod_ind, direction="both")
BIC_v1_FB <- stepwise(v1_mod_ind, k=log(nrow(v1)), direction="both")

#grafico AIC both
plot(AIC_v1_FB)
title(main = "UG Both AIC cultivar v1")

#grafico BIC both
plot(BIC_v1_FB)
title(main = "UG Both BIC cultivar v1")

# Matrice di correlazione
varCor <- cov2cor(CovV1)

# Graphical Lasso
var_lasso <- glasso(varCor, rho = 0.3)

# Matrice di adiacenza booleana
AM <- var_lasso$wi != 0
diag(AM) <- FALSE

# Costruzione del grafo igraph
graf_lasso <- graph_from_adjacency_matrix(
  AM,
  mode = "undirected",
  diag = FALSE
)

# Etichette dei nodi
V(graf_lasso)$name <- colnames(v1)

plot(main="UG glasso cultivar v1 con rho= 0.3", 
     graf_lasso,
     layout = layout_with_kk,
)

#----------------------------------------
#V2
v2_mod_sat <- cmod(~ .^., data=v2)
v2_mod_ind <- cmod(~.^1, data=v2)

#garfico usando penalizzazione AIC Forward
AIC_v2_F <- stepwise(v2_mod_ind, direction="forward")
plot(AIC_v2_F, "neato")
title(main="UG Forward AIC cultivar v2")

#grafico usando AIC Backward
AIC_v2_B <- stepwise(v2_mod_sat)
plot(AIC_v2_B, "neato")
title(main = "UG Backward AIC cultivar v2")

#grafico usando BIC forward
BIC_v2_F <- stepwise(v2_mod_ind, direction = "forward", k=log(nrow(v2)))
plot(BIC_v2_F, "neato")
title(main = "UG Forward BIC cultivar v2")

#grafico usando BIC backward
BIC_v2_B <- stepwise(v2_mod_sat, k=log(nrow(v2)))
plot(BIC_v2_B)
title(main="UG Backward BIC cultivar v2")

#Both directions
AIC_v2_FB <- stepwise(v2_mod_ind, direction="both")
BIC_v2_FB <- stepwise(v2_mod_ind, k=log(nrow(v2)), direction="both")

#grafico AIC both
plot(AIC_v2_FB)
title(main = "UG Both AIC cultivar v2")

#grafico BIC both
plot(BIC_v2_FB)
title(main = "UG Both BIC cultivar v2")

# Matrice di correlazione
varCor <- cov2cor(CovV2)

# Graphical Lasso
var_lasso <- glasso(varCor, rho = 0.2)

# Matrice di adiacenza booleana
AM <- var_lasso$wi != 0
diag(AM) <- FALSE

# Costruzione del grafo igraph
graf_lasso <- graph_from_adjacency_matrix(
  AM,
  mode = "undirected",
  diag = FALSE
)

# Etichette dei nodi
V(graf_lasso)$name <- colnames(v2)

plot(main="UG glasso cultivar v2 con rho= 0.3", 
     graf_lasso,
     layout = layout_with_kk,
)

#----------------------------------------
#V3
v3_mod_sat <- cmod(~ .^., data=v3)
v3_mod_ind <- cmod(~.^1, data=v3)

#garfico usando penalizzazione AIC Forward
AIC_v3_F <- stepwise(v3_mod_ind, direction="forward")
plot(AIC_v3_F, "neato")
title(main="UG Forward AIC cultivar v3")

#grafico usando AIC Backward
AIC_v3_B <- stepwise(v3_mod_sat)
plot(AIC_v3_B, "neato")
title(main = "UG Backward AIC cultivar v3")

#grafico usando BIC forward
BIC_v3_F <- stepwise(v3_mod_ind, direction = "forward", k=log(nrow(v3)))
plot(BIC_v3_F, "neato")
title(main = "UG Forward BIC cultivar v3")

#grafico usando BIC backward
BIC_v3_B <- stepwise(v3_mod_sat, k=log(nrow(v3)))
plot(BIC_v3_B)
title(main="UG Backward BIC cultivar v3")

#Both directions
AIC_v3_FB <- stepwise(v3_mod_ind, direction="both")
BIC_v3_FB <- stepwise(v3_mod_ind, k=log(nrow(v3)), direction="both")

#grafico AIC both
plot(AIC_v3_FB)
title(main = "UG Both AIC cultivar v3")

#grafico BIC both
plot(BIC_v3_FB)
title(main = "UG Both BIC cultivar v3")

# Matrice di correlazione
varCor <- cov2cor(CovV3)

# Graphical Lasso
var_lasso <- glasso(varCor, rho = 0.3)

# Matrice di adiacenza booleana
AM <- var_lasso$wi != 0
diag(AM) <- FALSE

# Costruzione del grafo igraph
graf_lasso <- graph_from_adjacency_matrix(
  AM,
  mode = "undirected",
  diag = FALSE
)

# Etichette dei nodi
V(graf_lasso)$name <- colnames(v3)

plot(main="UG glasso cultivar v3 con rho= 0.3", 
     graf_lasso,
     layout = layout_with_kk,
)

#--------------------------------------------------------------
#Grafi diretti (DAG)
# install.packages("bnlearn")
# install.packages("ggm")
# install.packages("graph")

#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("graph")

library(bnlearn)
library(graph)
library(ggm)


# Convertire tutte le colonne integer in numeric
wine_dataset <- data.frame(lapply(wine_dataset, function(x) {
  if (is.integer(x)) as.numeric(x) else x
}))

#Creazione grafico DAG intera popolazione BIC
DAG_pop <- hc(wine_dataset, score="bic-g")
plot(DAG_pop, main="DAG intera popolazione BIC Score")


DAG_pop <- amat(DAG_pop)   # amat() restituisce la matrice binaria del DAG

# Assicurati che la matrice abbia nomi
rownames(DAG_pop) <- colnames(wine_dataset)
colnames(DAG_pop) <- colnames(wine_dataset)
fdag <- fitDag(DAG_pop, CovPop, nrow(wine_dataset))

fdag$dev
fdag$df
#fdag$Shat
#fdag$Ahat
#fdag$Dhat

v1 <-data.frame(lapply(v1, function(x) {
  if (is.integer(x)) as.numeric(x) else x
})) 

#Creazione grafico DAG cultivar v1 BIC
DAG_v1 <- hc(v1)
plot(DAG_v1, main="DAG cultivar v1")

DAG_v1 <- amat(DAG_v1)   # amat() restituisce la matrice binaria del DAG

# Assicurati che la matrice abbia nomi
rownames(DAG_v1) <- colnames(v1)
colnames(DAG_v1) <- colnames(v1)
fdag <- fitDag(DAG_v1, CovV1, nrow(v1))

fdag$dev
fdag$df

v2 <-data.frame(lapply(v2, function(x) {
  if (is.integer(x)) as.numeric(x) else x
})) 

#Creazione grafico DAG cultivar v2 BIC
DAG_v2 <- hc(v2)
plot(DAG_v2, main="DAG cultivar v2")

DAG_v2 <- amat(DAG_v2)   # amat() restituisce la matrice binaria del DAG

# Assicurati che la matrice abbia nomi
rownames(DAG_v2) <- colnames(v2)
colnames(DAG_v2) <- colnames(v2)
fdag <- fitDag(DAG_v2, CovV2, nrow(v2))

fdag$dev
fdag$df

v3 <-data.frame(lapply(v3, function(x) {
  if (is.integer(x)) as.numeric(x) else x
})) 

#Creazione grafico DAG cultivar v3 BIC
DAG_v3 <- hc(v3)
plot(DAG_v3, main="DAG cultivar v3")

DAG_v3 <- amat(DAG_v3)   # amat() restituisce la matrice binaria del DAG

# Assicurati che la matrice abbia nomi
rownames(DAG_v3) <- colnames(v3)
colnames(DAG_v3) <- colnames(v3)
fdag <- fitDag(DAG_v3, CovV3, nrow(v3))

fdag$dev
fdag$df

# Selezione delle variabili di background e target
backgnd_vars <- setdiff(names(wine_dataset), "Clri")
target_vars  <- c("Clri")

# Creazione della blacklist
blacklist <- expand.grid(
  from = target_vars,
  to   = backgnd_vars
)

# Creazione del modello bayesiano con variabile target specificata
#Intera popolazione
target_DAG_pop <- hc(
  wine_dataset,
  blacklist = blacklist
)
plot(target_DAG_pop, main="DAG intera popolazione con Clri target")

DAG_pop <- amat(target_DAG_pop)   # amat() restituisce la matrice binaria del DAG

# Assicurati che la matrice abbia nomi
rownames(DAG_pop) <- colnames(wine_dataset)
colnames(DAG_pop) <- colnames(wine_dataset)
fdag <- fitDag(DAG_pop, CovPop, nrow(wine_dataset))

fdag$dev
fdag$df

#Cultivar v1 con target
target_DAG_v1 <- hc(
  v1,
  blacklist = blacklist
)

plot(target_DAG_v1, main="DAG cultivar v1 con Clri target")

DAG_v1 <- amat(target_DAG_v1)   # amat() restituisce la matrice binaria del DAG

# Assicurati che la matrice abbia nomi
rownames(DAG_v1) <- colnames(wine_dataset)
colnames(DAG_v1) <- colnames(wine_dataset)
fdag <- fitDag(DAG_v1, CovV1, nrow(v1))

fdag$dev
fdag$df

#Cultivar v2 con target
target_DAG_v2 <- hc(
  v2,
  blacklist = blacklist
)

plot(target_DAG_v2, main="DAG cultivar v2 con Clri target")

DAG_v2 <- amat(target_DAG_v2)   # amat() restituisce la matrice binaria del DAG

# Assicurati che la matrice abbia nomi
rownames(DAG_v2) <- colnames(wine_dataset)
colnames(DAG_v2) <- colnames(wine_dataset)
fdag <- fitDag(DAG_v2, CovV2, nrow(v2))

fdag$dev
fdag$df

#Cultivar v3 con target
target_DAG_v3 <- hc(
  v3,
  blacklist = blacklist
)

plot(target_DAG_v3, main="DAG cultivar v3 con Clri target")

DAG_v3 <- amat(target_DAG_v3)   # amat() restituisce la matrice binaria del DAG

# Assicurati che la matrice abbia nomi
rownames(DAG_v3) <- colnames(wine_dataset)
colnames(DAG_v3) <- colnames(wine_dataset)
fdag <- fitDag(DAG_v3, CovV3, nrow(v3))

fdag$dev
fdag$df

#----------------------------------------------
#Caso con Wine dataset

wine <-data.frame(lapply(wine, function(x) {
  if (is.integer(x)) as.numeric(x) else x
})) 

#Creazione grafico DAG intera popolazione con la variabile discreta cultivar BIC
DAG_wine <-hc(wine)
plot(DAG_wine, main="DAG wine dataset")


# Con conoscenze a priori
# Conversione di eventuali colonne integer in numeric
wine <- data.frame(lapply(wine, function(x) {
  if (is.integer(x)) as.numeric(x) else x
}))

# Definizione delle variabili target e esogene
target_vars <- c("Clri")   # variabile target
exogenous_vars <- c("Cult") # variabile esogena

# Tutte le altre variabili
other_vars <- setdiff(names(wine), c(target_vars, exogenous_vars))

# Creazione della blacklist
# 1) Target non può influenzare nessun'altra variabile
# 2) Variabile esogena non può essere influenzata da nessun'altra
blacklist <- rbind(
  expand.grid(from = target_vars, to = other_vars),
  expand.grid(from = other_vars, to = exogenous_vars),
  expand.grid(from = target_vars, to = exogenous_vars)
)

# Apprendimento del DAG con Hill-Climbing usando BIC-CG
DAG_wine <- hc(wine, score = "bic-cg", blacklist = blacklist)

# Visualizzazione del DAG
plot(DAG_wine, main = "DAG Wine dataset con target Clri e Cult esogena")

#--------------------------------------------------------------
#Stimare un modello predittivo

#--------------------------------------------
#Intera Popolazione

#definiamo un modello nullo
null_model <- lm ( Clri ~ 1, data = wine_dataset)

#definiamo un modello saturo
full_model <-lm(Clri ~ . , data = wine_dataset)

# Definizioni dello scope per definire gli intervalli di lavoro
scope <- list ( lower = formula ( null_model ), upper = formula ( full_model ))

#AIC

#backward
back_model_AIC <- step(full_model, scope = scope, direction = "backward")
summary(back_model_AIC)

#forward
forw_model_AIC <- step(null_model, scope = scope, direction = "forward")
summary(forw_model_AIC)

#both
both_model_AIC <- step(null_model, scope = scope, direction = "both")
summary(both_model_AIC)

#BIC
#backward
back_model_BIC <-step(full_model, scope=scope, direction = "backward", 
                      k=log(nrow(wine_dataset)))
summary(back_model_BIC)

#forward
forw_model_BIC <- step(null_model, scope=scope, direction = "forward", 
                       k=log(nrow(wine_dataset)))
summary(forw_model_BIC)

#both
both_model_BIC <- step(null_model, scope=scope, direction = "both", 
                       k=log(nrow(wine_dataset)))
summary(both_model_BIC)
#--------------------------------------------
#Cultivar v1

#definiamo un modello nullo
null_model <- lm ( Clri ~ 1, data = v1)

#definiamo un modello saturo
full_model <-lm(Clri ~ . , data = v1)

# Definizioni dello scope per definire gli intervalli di lavoro
scope <- list ( lower = formula ( null_model ), upper = formula ( full_model ))

#AIC

#backward
back_model_AIC <- step(full_model, scope=scope, direction = "backward")
summary(back_model_AIC)

#forward
forw_model_AIC <- step(null_model, scope=scope, direction = "forward")
summary(forw_model_AIC)

#both
both_model_AIC <- step(null_model, scope=scope, direction = "both")
summary(both_model_AIC)

#BIC
#backward
back_model_BIC <-step(full_model, scope = scope, direction = "backward", 
                      k=log(nrow(v1)))
summary(back_model_BIC)

#forward
forw_model_BIC <- step(null_model, scope = scope, direction = "forward", 
                       k=log(nrow(v1)))
summary(forw_model_BIC)

#both
both_model_BIC <- step(null_model, scope = scope, direction = "both", 
                       k=log(nrow(v1)))
summary(both_model_BIC)

#--------------------------------------------
#Cultivar v2

#definiamo un modello nullo
null_model <- lm ( Clri ~ 1, data = v2)

#definiamo un modello saturo
full_model <-lm(Clri ~ . , data = v2)

# Definizioni dello scope per definire gli intervalli di lavoro
scope <- list ( lower = formula ( null_model ), upper = formula ( full_model ))

#AIC

#backward
back_model_AIC <- step(full_model, scope = scope, direction = "backward")
summary(back_model_AIC)

#forward
forw_model_AIC <- step(null_model, scope = scope, direction = "forward")
summary(forw_model_AIC)

#both
both_model_AIC <- step(null_model, scope = scope, direction = "both")
summary(both_model_AIC)

#BIC
#backward
back_model_BIC <-step(full_model, scope = scope, direction = "backward", 
                      k=log(nrow(v2)))
summary(back_model_BIC)

#forward
forw_model_BIC <- step(null_model, scope = scope, direction = "forward", 
                       k=log(nrow(v2)))
summary(forw_model_BIC)

#both
both_model_BIC <- step(null_model, scope = scope, direction = "both", 
                       k=log(nrow(v2)))
summary(both_model_BIC)

#--------------------------------------------
#Cultivar v3

#definiamo un modello nullo
null_model <- lm ( Clri ~ 1, data = v3)

#definiamo un modello saturo
full_model <-lm(Clri ~ . , data = v3)

# Definizioni dello scope per definire gli intervalli di lavoro
scope <- list ( lower = formula ( null_model ), upper = formula ( full_model ))

#AIC

#backward
back_model_AIC <- step(full_model, scope = scope, direction = "backward")
summary(back_model_AIC)

#forward
forw_model_AIC <- step(null_model, scope = scope, direction = "forward")
summary(forw_model_AIC)

#both
both_model_AIC <- step(null_model, scope=scope, direction = "both")
summary(both_model_AIC)

#BIC
#backward
back_model_BIC <-step(full_model, scope = scope, direction = "backward", 
                      k=log(nrow(v3)))
summary(back_model_BIC)

#forward
forw_model_BIC <- step(null_model, scope = scope, direction = "forward", 
                       k=log(nrow(v3)))
summary(forw_model_BIC)

#both
both_model_BIC <- step(null_model, scope = scope, direction = "both", 
                       k=log(nrow(v3)))
summary(both_model_BIC)

#--------------------------------------------
#Wine dataset

#definiamo un modello nullo
null_model <- lm ( Clri ~ 1, data = wine)

#definiamo un modello saturo
full_model <-lm(Clri ~ . , data = wine)

# Definizioni dello scope per definire gli intervalli di lavoro
scope <- list ( lower = formula ( null_model ), upper = formula ( full_model ))

#AIC

#backward
back_model_AIC <- step(full_model, scope = scope, direction = "backward")
summary(back_model_AIC)

#forward
forw_model_AIC <- step(null_model, scope = scope, direction = "forward")
summary(forw_model_AIC)

#both
both_model_AIC <- step(null_model, scope = scope, direction = "both")
summary(both_model_AIC)

#BIC
#backward
back_model_BIC <-step(full_model, scope = scope, direction = "backward", 
                      k=log(nrow(wine)))
summary(back_model_BIC)

#forward
forw_model_BIC <- step(null_model, scope = scope, direction = "forward", 
                       k=log(nrow(wine)))
summary(forw_model_BIC)

#both
both_model_BIC <- step(null_model, scope = scope, direction = "both", 
                       k=log(nrow(wine)))
summary(both_model_BIC)


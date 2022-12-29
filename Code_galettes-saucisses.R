################################################################################
### LOAD DATA
################################################################################
setwd("/Users/niels/Desktop/Cours/hackhaton")
data <- read.csv('data.csv', sep= ",", header = TRUE, dec= ".")

################################################################################
### CHARGEMENT PACKAGES
################################################################################

require("tidyverse")
require("sf")
require(geosphere)
require(flexmix)
require(FactoMineR)

################################################################################
### ASSIGNATION D'UNE ID PAR RESTAU
################################################################################

L <- c(rep(0,600))
for (i in 1:600){
  print(i)
  L[i] <- i
  print(L[i])
}
data_count <- data %>%
  count(start_lat,start_lon) %>% 
  mutate(ID = L)

p <- ggplot(data, aes(x=fee)) + 
  geom_density()
p

dt <- left_join(data,data_count, by=c("start_lat","start_lon"))

################################################################################
### CALCUL DISTANCE
################################################################################

p1 <- c(dt$start_lon[1], dt$start_lat[1])
p2 <- c(dt$end_lon[1], dt$end_lat[1])
distGeo(p1,p2) 
#distances dans le jdd
for (k in (1:4800)){
  p1 <- c(dt$start_lon[k], dt$start_lat[k])
  p2 <- c(dt$end_lon[k], dt$end_lat[k])
  dt$distance[k] <- distGeo(p1,p2) 
}

hist(dt$distance)

# choisis en fonct de la variabilité géographique de ses clients 
#création colonne algorithm
###dataframe a utiliser
#dt

################################################################################
### AJOUT TEMPS & METEO
################################################################################

heat <- as.factor(data$heat)
modheat <- LinearModel(data$fee~heat)
modheat
rain <- as.factor(data$rain)
modrain <- LinearModel(data$fee~rain)
modrain


modfeedist <- LinearModel(dt$fee~dt$distance)
modfeedist

dt$hour <- as.numeric(dt$hour)
data2 <- dt %>%
  mutate(hour=hour*60) %>%
  mutate((Time=hour+minute-720))


data2$min <- data2$`(Time = hour + minute - 720)`
data2$min <- round(data2$min, -1)

modmin <- LinearModel(dt$fee~dt$min)
modmin

modfeedloc <- LinearModel(dt$fee~dt$distance)
modfeedist

plot(data2$min, data2$fee)

hist(data2$min)

dt$min <- data2$min

################################################################################
### AJOUT CLASSES SOCIALES ET WEEEKEND/SEMAINE
################################################################################

faible <- c(75019,75020,75010, 75018)
moyen <- c(75014, 75013, 75012, 75001, 75002, 75003, 75004, 75011)
fort <- c(75016, 75015, 75017, 75008, 75007, 75006, 75005, 75009, 75116)

dt$social_resto <- rep(NA, 4800)

for (i in 1:dim(dt)[1]) {
  if (dt$start_code_postal[i] %in% faible) {
    dt$social_resto[i] <- 0
  }
  if (dt$start_code_postal[i] %in% moyen) {
    dt$social_resto[i] <- 5
  }
  if (dt$start_code_postal[i] %in% fort) {
    dt$social_resto[i] <- 10
  }
}

dt$social_client <- rep(NA, 4800)

for (i in 1:4800) {
  if (dt$end_code_postal[i] %in% faible) {
    dt$social_client[i] <- 0
  }
  if (dt$end_code_postal[i] %in% moyen) {
    dt$social_client[i] <- 5
  }
  if (dt$end_code_postal[i] %in% fort) {
    dt$social_client[i] <- 10
  }
}

semaine <- c("Monday","Tuesday","Wednesday","Thursday")
weekend <- c("Friday","Saturday","Sunday")

dt$jour <- rep(NA, 4800)

for (i in 1:4800) {
  if (dt$day_of_week[i] %in% semaine) {
    dt$jour[i] <- 1
  }
  if (dt$day_of_week[i] %in% weekend) {
    dt$jour[i] <- 5
  }
}

dt_distance <- dt %>%
  group_by(ID, start_code_postal) %>%
  summarise(mean_dist= mean(distance), sd_dist= sd(distance))

dt <- cbind(dt, dt_distance[,3:4])

################################################################################
### DENSITE DE RESTO PAR LOCALISATION
################################################################################

data_code <-  dt %>%
  count(start_code_postal) %>%
  mutate(nb_code_postal_start = n/8) # on peut diviser par 8 car chaque resto est present sur 8 lignes

sum(data_code$nb_code_postal_start) #on retrouve 600 restos

dt <- inner_join(dt,data_code[,-2], by=c("start_code_postal")) 

ggplot(dt, aes(x=nb_code_postal_start, y=fee)) +
  geom_point()

dt$nb_code_postal_start <- as.factor(dt$nb_code_postal_start)
ggplot(dt, aes(x=nb_code_postal_start, y=fee)) +
  geom_boxplot()


################################################################################
### FLEXMIX
################################################################################

tri1 <- function(table){
  purs <- c()
  for (k in (1:length(table[1,]))){
    if (table[1,k] == 8){
      purs <- c(purs,k)
    }
  }
  return(purs)
}

tri2 <- function(table){
  purs <- c()
  for (k in (1:length(table[1,]))){
    if (table[1,k] == 0){
      purs <- c(purs,k)
    }
  }
  return(purs)
}

tri3 <- function(table){
  purs <- c()
  for (k in (1:length(table[1,]))){
    if (table[1,k] > 6){
      purs <- c(purs,k)
    }
  }
  return(purs)
}

tri4 <- function(table){
  purs <- c()
  for (k in (1:length(table[1,]))){
    if (table[1,k] < 3){
      purs <- c(purs,k)
    }
  }
  return(purs)
}

tri1large <- function(table){
  purs <- c()
  for (k in (1:length(table[1,]))){
    if (table[1,k] < 8){ # full classe sur 1 pour le 8
      purs <- c(purs,k)
    }
  }
  return(purs)
}

tri2large <- function(table){
  purs <- c()
  for (k in (1:length(table[1,]))){
    if (table[1,k] > 0){ # full classe sur 2 pour le 8 si 0 un
      purs <- c(purs,k)
    }
  }
  return(purs)
}


######

dt_tri <- dt[order(dt$ID),]

######

## On classifie tous les restaurants qui ont des frais fixes ensemble

pur <- c()
i=1
g0=c()
while (i<=4800){
  if ((dt_tri[i,"fee"]*8) == sum(dt_tri[i:(i+7),"fee"])){
    g <- cbind(dt_tri[i:(i+7),1:22])
    g0 <- rbind(g0,g)
    pur <- c(pur,dt_tri[i,"ID"])
  }
  i = i + 8
  print(i)
}

g0$a <- rep(0,length(g0[,1]))

a <- c(rep(0,length(dt_tri[,1])))

for (i in pur){
  for (j in 1:8) a[(i-1)*8+j]=2
}

dt_impur <- cbind(dt_tri,a)

dt_impur <- dt_impur[a==0,]

######

## Nous procédons ensuite à une classification via flexmix de façon progressive, en changeant les variables explicatives
## pour chaque étape, car les restaurants ne classifient pas de la même façon. Nous sélectionnons à chaque fois les restaurants
## que nous définissons comme "purs", c'est à dire qui ont les 8 commandes classifiées pareil à chaque flexmix. Pour le dernier
## flexmix (le 3e), nous séparons juste en 2 groupes.

m1 <- flexmix(fee ~ min+jour,data = dt_impur, k = 2)

summary(m1)

print(plot(m1))

clusters <- clusters(m1)

visualisation1 <- table(clusters(m1),dt_impur$ID)

purs1 <- tri1(visualisation1)
purs2 <- tri2(visualisation1)

a <- c(rep(0,length(dt_impur[,1])))

for (i in purs1){
  for (j in 1:8) a[(i-1)*8+j]=1
}
g1 <- cbind(dt_impur,a)
g1 = g1[a==1,]

a <- c(rep(0,length(dt_impur[,1])))

for (i in purs2){
  for (j in 1:8) a[(i-1)*8+j]=2
}

g2 <- cbind(dt_impur,a)
g2 = g2[a==2,]

impur <- c(purs1,purs2)

a <- c(rep(0,length(dt_impur[,1])))

for (i in impur){
  for (j in 1:8) a[(i-1)*8+j]=2
}

dt_impur <- cbind(dt_impur,a)

dt_impur <- dt_impur[a==0,]

#######

m2 <- flexmix(fee ~ distance+jour,data = dt_impur, k = 2)

visualisation2 <- table(clusters(m2),dt_impur$ID)

purs3 <- tri1(visualisation2)
purs4 <- tri2(visualisation2)

a <- c(rep(0,length(dt_impur[,1])))

for (i in purs3){
  for (j in 1:8) a[(i-1)*8+j]=3
}
g3 <- cbind(dt_impur,a)
g3 = g3[a==3,]

a <- c(rep(0,length(dt_impur[,1])))

for (i in purs4){
  for (j in 1:8) a[(i-1)*8+j]=4
}

g4 <- cbind(dt_impur,a)
g4 = g4[a==4,]

impur <- c(purs3,purs4)

a <- c(rep(0,length(dt_impur[,1])))

for (i in impur){
  for (j in 1:8) a[(i-1)*8+j]=2
}

dt_impur <- cbind(dt_impur,a)

dt_impur <- dt_impur[a==0,]

######

m3 <- flexmix(fee ~ distance+rain+min+heat+social_resto+jour,data = dt_impur, k = 2)

visualisation3 <- table(clusters(m3),dt_impur$ID)

purs5 <- tri2(visualisation3)
purs6 <- tri2large(visualisation3)

## L'algorithme ne classifie pas toujours dans le même ordre, on remet donc bien dans l'ordre voulu

if ((is.null(purs5)) | (is.null(purs6))){
  purs5 <- tri1(visualisation3)
  purs6 <- tri1large(visualisation3)
}

a <- c(rep(0,length(dt_impur[,1])))

for (i in purs5){
  for (j in 1:8) a[(i-1)*8+j]=5
}
g5 <- cbind(dt_impur,a)
g5 = g5[a==5,]

a <- c(rep(0,length(dt_impur[,1])))

for (i in purs6){
  for (j in 1:8) a[(i-1)*8+j]=6
}

g6 <- cbind(dt_impur,a)
g6 = g6[a==6,]

impur <- c(purs5,purs6)

a <- c(rep(0,length(dt_impur[,1])))

for (i in impur){
  for (j in 1:8) a[(i-1)*8+j]=2
}

dt_impur <- cbind(dt_impur,a)

dt_impur <- dt_impur[a==0,]

#####

length(g0[,1])+length(g1[,1])+length(g2[,1])+length(g3[,1])+length(g4[,1])+length(g5[,1])+length(g6[,1])
soumission <- data.frame(observation_uuid = c(g0[,1],g1[,1],g2[,1],g3[,1],g4[,1],g5[,1],g6[,1]),
                         algorithm = c(g0[,23],g1[,25],g2[,25],g3[,26],g4[,26],g5[,27],g6[,27]))
write.csv(soumission, "soumission_finale_v3.csv",row.names = FALSE)

# 
# #####
# 
# m4 <- flexmix(fee ~ distance+rain+min+heat+social_client,data = dt_impur, k = 2)
# 
# visualisation4 <- table(clusters(m4),dt_impur$ID)
# 
# purs7 <- tri1large(visualisation4)
# purs8 <- tri2large(visualisation4)
# 
# a <- c(rep(0,length(dt_impur[,1])))
# 
# for (i in purs7){
#   for (j in 1:8) a[(i-1)*8+j]=7
# }
# g7 <- cbind(dt_impur,a)
# g7 = g7[a==7,]
# 
# a <- c(rep(0,length(dt_impur[,1])))
# 
# for (i in purs8){
#   for (j in 1:8) a[(i-1)*8+j]=8
# }
# 
# g8 <- cbind(dt_impur,a)
# g8 = g8[a==8,]
# 
# length(g0[,1])+length(g1[,1])+length(g2[,1])+length(g3[,1])+length(g4[,1])+length(g5[,1])+length(g6[,1])+length(g7[,1])+length(g8[,1])
# 
# soumission <- rbind(g0[,c(1,23)],g1[,c(1,24)],g2[,c(1,24)],g3[,c(1,25)],g4[,c(1,25)],g5[,c(1,26)],g6[,c(1,26)],g7[,c(1,27)],g8[,c(1,27)])
# colnames(soumission) <- c("observation_uuid","algorithm")
# write.table(soumission, "soumission_finale_v2.csv", sep=",",row.names = FALSE, dec = ".", na= "NA")
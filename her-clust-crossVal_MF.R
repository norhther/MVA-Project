library(cluster)
library(NbClust)

library(factoextra)
library(FactoMineR)
library(fpc)
library(NbClust)
library(rgl)
library(tidyverse)
library(tidymodels)
library(lubridate)
#library(janitor)

library(dendextend)

# Load data
# setwd("/home/martin/personal/Master/MVA/project/MVA-Project")
df <- read_csv("df_preprocessed.csv")

# Transform to factors
df$Sex <- factor(df$Sex)
df$Event <- factor(df$Event)
df$Equipment <- factor(df$Equipment)
#df$Place <- factor(df$Place)
df$Place <- factor(fct_other(df$Place,keep=c(1,2,3,"DQ")),levels=c(1,2,3,"Other","DQ"),order=T)
df$Tested <- factor(df$Tested)
df$Country <- factor(df$Country)
top_countries <- names(sort(table(df$Country),decreasing = TRUE)[1:10])
df$Country <- fct_other(df$Country,keep=top_countries)
df$Federation <- factor(df$Federation)
df$MeetCountry <- fct_other(df$MeetCountry,keep=top_countries)
df$MeetName <- factor(df$MeetName)
df$Year <- year(df$Date)

# Rename vars with ? because it causes problems
names(df) <- gsub("(.*)\\?", "\\1", names(df))

df <- df %>% mutate('LiftedBench1Kg' = as_factor(df$'LiftedBench1Kg'),
                    'LiftedBench2Kg' = as_factor(df$'LiftedBench2Kg'),
                    'LiftedBench3Kg' = as_factor(df$'LiftedBench3Kg'),
                    'LiftedSquat1Kg' = as_factor(df$'LiftedSquat1Kg'),
                    'LiftedSquat2Kg' = as_factor(df$'LiftedSquat2Kg'),
                    'LiftedSquat3Kg' = as_factor(df$'LiftedSquat3Kg'),
                    'LiftedDeadlift1Kg' = as_factor(df$'LiftedDeadlift1Kg'),
                    'LiftedDeadlift2Kg' = as_factor(df$'LiftedDeadlift2Kg'),
                    'LiftedDeadlift3Kg' = as_factor(df$'LiftedDeadlift3Kg'))

num_to_cat <- df %>%
  transmute(
    Age_class = ifelse(
    Age < 12, "Kid", ifelse(
    Age < 18, "Teenager", ifelse(
    Age < 40, "Regular", "Veteran"))),
    BodyWeight_class = ifelse(
    BodyweightKg < 61, "61", ifelse(
    BodyweightKg < 67, "67", ifelse(
    BodyweightKg < 73, "73", ifelse(
    BodyweightKg < 81, "81", ifelse(
    BodyweightKg < 96, "96", ifelse(
    BodyweightKg < 109, "109", ifelse(
    BodyweightKg < 10000, "109+", "Error"
  )))))))) %>%
  mutate(Age_class = as_factor(Age_class),
          BodyWeight_class = as_factor(BodyWeight_class))
df$Age_class = num_to_cat$Age_class
df$BodyWeight_class = num_to_cat$BodyWeight_class

summary(df)

# Get only 2019
df_1y = df[df$Year==2019,]
nrow(df_1y)

# No sex distinction
df_1ym = df_1y
#df_1ym = df_1y[df_1y$Sex == "M",]


# Sample only 25% of values
set.seed(12345)
getnrow = as.integer(nrow(df_1ym)*0.25)
# Balance Male vs Female
sex_prop <- sum(df_1ym$Sex == "F")/sum(df_1ym$Sex == "M")
prob <- ifelse(df_1ym$Sex == "M", max(1.0, sex_prop), max(1.0, 1.0/sex_prop))
df_short <- df_1ym[sample(1:nrow(df_1ym), getnrow, prob=prob), ] 

names(df_short)
nrow(df_short)
summary(df_short)

# dfc1 <- df_short[,c(5,6, 2,20, 7:15)] # Raw results
dfc1 <- df_short[,c(5,6, 2,20, 25:33,34:42)] # Absolute value
# dfc1 <- df_short[,c(5,6, 2,20, 43:51,34:42)] # Increases

summary(dfc1)

# Comput distances
gower.dist1 <- daisy(dfc1, metric = c("gower"))

# Cluster
m="ward.D"
aggl.clust1 <- hclust(gower.dist1, method = m)
plot(aggl.clust1, labels=F,
     main = paste("Agglomerative, ",m," linkages",sep=""))
rect.hclust(aggl.clust1, k=4, border=6)

gdist = gower.dist1
for (m in c("ward.D", "ward.D2", "single")){
  aggl.clust <- hclust(gdist, method = m)
  plot(aggl.clust,
      main = paste("Agglomerative, ",m," linkages",sep=""))
}

# Find k
#indices <- c("frey", "mcclain", "cindex", "silhouette", "dunn")
indices <- c("silhouette", "dunn")
particiones <- vector()
for(i in 1:length(indices)){
  print(indices[i])
  sel_k<-NbClust(diss=gower.dist1,distance=NULL,min.nc=3,max.nc=10,method="ward.D",index=indices[i])
  particiones[i]<-max(sel_k$Best.partition)
}
names(particiones) <- indices
particiones
# M&F
# For dfc1:     1          2          6          2          2
# For dfc3:     1          2          6          4          3

# Cutting the tree
df_short$cluster = as.factor(cutree(tree=aggl.clust1, k=4))
gather_short <- tidyr::gather(data=df_short,
                                key="Method",value="Cluster",
                                cluster)

p <- ggplot(gather_short) + aes(x=factor(Cluster),fill=Place) +
    geom_bar(position="fill") +
    facet_grid(~Method) 
p


df_foo <- dfc1
df_foo$Place <- df_short$Place

df_foo$cluster <- df_short$cluster

ncol(df_foo)
catdes(df_foo,24)

saveRDS(as.data.frame(df_short), file = "hcluster/hcluster_2019-p25-tr-k4.rds")

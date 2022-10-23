# My try to do pca
library(FactoMineR)
library(factoextra)
library(corrplot)
library(tidyverse)
library(Factoshiny)
library(dplyr)


df <- read_csv("/home/martin/personal/Master/MVA/project/MVA-Project/df_preprocessed.csv")

# Set Lifted*? as factor
df <- df %>% mutate('LiftedBench1Kg?' = as_factor(df$'LiftedBench1Kg?'),
              'LiftedBench2Kg?' = as_factor(df$'LiftedBench2Kg?'),
              'LiftedBench3Kg?' = as_factor(df$'LiftedBench3Kg?'),
              'LiftedSquat1Kg?' = as_factor(df$'LiftedSquat1Kg?'),
              'LiftedSquat2Kg?' = as_factor(df$'LiftedSquat2Kg?'),
              'LiftedSquat3Kg?' = as_factor(df$'LiftedSquat3Kg?'),
              'LiftedDeadlift1Kg?' = as_factor(df$'LiftedDeadlift1Kg?'),
              'LiftedDeadlift2Kg?' = as_factor(df$'LiftedDeadlift2Kg?'),
              'LiftedDeadlift3Kg?' = as_factor(df$'LiftedDeadlift3Kg?'),
              Sex = as_factor(Sex),
              #Event = as_factor(Event),  #Useles
              Equipment = as_factor(Equipment),
              Tested = as_factor(Tested),
              Federation = as_factor(Federation),
              Place = factor(Place,levels=c(1:9,"Other","DQ"))
              )

top_country = df %>% group_by(df$Country) %>% 
        summarize(Count=n()) %>% 
        mutate(Percent = round((Count/sum(Count)*100))) %>% 
        arrange(desc(Count))
####
# W would like to add country as factor.
# Use Only 22 most freq countries
# Group rest in other


# Computes WeightInc as diff with previous attempt
df_inc = subset(
          transform(df,
            WeightIncSquat2 = WeightTriedSquat2Kg - WeightTriedSquat1Kg,
            WeightIncSquat3 = WeightTriedSquat3Kg - WeightTriedSquat2Kg,
            WeightIncBench2 = WeightTriedBench2Kg - WeightTriedBench1Kg,
            WeightIncBench3 = WeightTriedBench3Kg - WeightTriedBench2Kg,
            WeightIncDeadlift2 = WeightTriedDeadlift2Kg - WeightTriedDeadlift1Kg,
            WeightIncDeadlift3 = WeightTriedDeadlift3Kg - WeightTriedDeadlift2Kg      
            ),
          select=-c(Squat1Kg,Squat2Kg,Squat3Kg,
                    Bench1Kg,Bench2Kg,Bench3Kg,
                    Deadlift1Kg,Deadlift2Kg,Deadlift3Kg,
                    WeightTriedSquat2Kg,WeightTriedSquat3Kg,
                    WeightTriedBench2Kg,WeightTriedBench3Kg,
                    WeightTriedDeadlift2Kg,WeightTriedDeadlift3Kg))

df_cat = df_inc[c(2,3,8,10,12,19:27,5:7,16:18,28:33)]

# Rename Columns for Viz
names(df_cat) = c("Sex","Event","Place","Tested","Federation", "S_L1","S_L2","S_L3", "B_L1","B_L2","B_L3", "D_L1","D_L2","D_L3","Age","BodyW","TotKg","S_WT","B_WT","D_WT", "S_WI2","S_WI3","B_WI2","B_WI3","D_WI2","D_WI3")


####
#Select 20% of individuals
df_cat.s <- df_cat[sample(nrow(df_cat), size=nrow(df_cat)*0.1), ]
###

summary(df_cat)

# Listening on http://127.0.0.1:5058
# Warning: The select input "indsup" contains a large number of options;
# consider using server-side selectize for massively improved performance.
# See the Details section of the ?selectizeInput help topic.
df_cat

result <- Factoshiny(df_cat.s)
######
######


dec.pca = PCA(df_inc, scale.unit=TRUE, graph=F) 

# Numeric part: df_inc[c(5,6,7,16:18,28:33)]
# Category part: df_inc[c(2,4,10,12,19:27)]
df_pca = df_pca=df_inc[c(5,6,7,16:18,28:33,2,4,10,12,19:27)]

dec.pca = PCA(df_pca, scale.unit=TRUE, quali.sup=c(13:25), graph=F) 


summary(dec.pca)
print(dec.pca)
get_pca(dec.pca)

################################################EXPLORING PCA RESULTS
get_eigenvalue(dec.pca)
fviz_eig(dec.pca)
get_pca_ind(dec.pca) ## get_pca_var(dec.pca): Extract the results for individuals and variables, respectively.
#fviz_pca_ind(dec.pca) ## fviz_pca_var(dec.pca): Visualize the results individuals and variables, respectively.
fviz_pca_biplot(dec.pca) ## Make a biplot of individuals and variables.

plot.PCA(dec.pca,c(1,2),label=c("var"))
plot.PCA(dec.pca,c(1,3),label=c("var"))


##############
##############
##############
library(tidyverse)
library(tidymodels)
library(lubridate)
library(ggcorrplot)
library(Factoshiny)
library(FactoMineR)
library(factoextra)
library(arules)
library(RColorBrewer)
library(viridis)
library(ggrepel)
options(ggrepel.max.overlaps = Inf)


df <- read_csv("df_preprocessed.csv")

numeric_cols <- df %>%
  select_if(is.numeric)

cols_zero_var <- numeric_cols %>% 
  select(colnames(numeric_cols)[1:2],
         colnames(numeric_cols)[13],
         colnames(numeric_cols)[32:ncol(numeric_cols)],
         -TotalKg)

top10_countries <- fct_other(df$Country,keep=c(names(sort(table(df$Country),decreasing = TRUE)[1:10])))

## Use this for done/fail
#cols_zero_var <- df %>% 
#  select(colnames(numeric_cols)[c(1:2,32:49)])
## Only done
#cols_zero_var <- df %>% 
#  select(colnames(numeric_cols)[c(1:2,32,34,36,38,40,42,44,46,48)])
## Use this for as-it-is
#cols_zero_var <- df %>% 
#  select(colnames(numeric_cols)[c(1:11)])
## Use this for weight-tried
#cols_zero_var <- df %>% 
#  select(colnames(numeric_cols)[c(1:11)]) %>% mutate_if(is.numeric, abs)

num_to_cat <- cols_zero_var %>%
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

#res.pca <- PCA(cols_zero_var, graph = FALSE)
res.pca <- PCA(cols_zero_var, quali.sup=c("Goodlift"), graph = FALSE, ncp=6)



eig <- get_eig(res.pca)
fviz_screeplot(res.pca, addlabels = TRUE)
var <- get_pca_var(res.pca)

par(mfrow=c(1,1))
barplot(var$cor[,1],horiz=TRUE,las=2,cex.names=0.6)
barplot(var$contrib[,1],horiz=TRUE,las=2,cex.names=0.6)

barplot(var$cor[,2],horiz=TRUE,las=2,cex.names=0.6)
barplot(var$contrib[,2],horiz=TRUE,las=2,cex.names=0.6)

barplot(var$cor[,3],horiz=TRUE,las=2,cex.names=0.6)
barplot(var$contrib[,3],horiz=TRUE,las=2,cex.names=0.6)

barplot(var$cor[,4],horiz=TRUE,las=2,cex.names=0.6)
barplot(var$contrib[,4],horiz=TRUE,las=2,cex.names=0.6)

barplot(var$cor[,5],horiz=TRUE,las=2,cex.names=0.6)
barplot(var$contrib[,5],horiz=TRUE,las=2,cex.names=0.6)

barplot(var$cor[,6],horiz=TRUE,las=2,cex.names=0.6)
barplot(var$contrib[,6],horiz=TRUE,las=2,cex.names=0.6)

fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
fviz_pca_var(res.pca,c(1,2), col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
fviz_pca_var(res.pca,c(2,3), col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
fviz_pca_var(res.pca,c(2,4), col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
fviz_pca_var(res.pca,c(2,5), col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

# Contributions of variables to PC1 & PC2
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10)


# This breaks
## Color individuals?
#fviz_pca_ind(res.pca, label = "none" ,
#             habillage = "Goodlift",
#             palette = "grey"
#             )

plane=c(1,3)
fviz_pca_ind(res.pca,plane,
             label = "none", # hide individual labels
             habillage = as_factor(df$Sex), # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             alpha.ind = 0.2
)
fviz_pca_ind(res.pca,plane,
             label = "none", # hide individual labels
             habillage = num_to_cat$Age_class, # color by groups
             palette = c("#FF4754", "#EBF056", "#5AE84A","#1866BA"),
             addEllipses = TRUE, # Concentration ellipses
             alpha.ind = 0.2
)
fviz_pca_ind(res.pca,plane,
             label = "none", # hide individual labels
             #geom="none",
             habillage = fct_other(df$Place,keep=c(1,2,3,"DQ")), # color by groups
             palette = c("#FF4754", "#EBF056", "#5AE84A","#C757A0","#1866BA"),
             addEllipses = TRUE, # Concentration ellipses
             alpha.ind = 0.05
)
fviz_pca_ind(res.pca,plane,
             label = "none", # hide individual labels
             habillage = num_to_cat$BodyWeight_class, # color by groups
             #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             alpha.ind = 0.2
)

plane=c(2,3)
fviz_pca_ind(res.pca,plane,
             label = "none", # hide individual labels
             habillage = as_factor(df$Equipment), # color by groups
             #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             alpha.ind = 0.2
)
fviz_pca_ind(res.pca,plane,
             label = "none", # hide individual labels
             habillage = top10_countries, # color by groups
             #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             alpha.ind = 0.2
)

####### BIPLOTS
fviz_pca_biplot(res.pca,c(1,2),
                label = "var", # hide individual labels
                repel = TRUE,
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                alpha.ind = 0.1
)

fviz_pca_biplot(res.pca,c(1,3),
             label = "var", # hide individual labels
             repel = TRUE,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             alpha.ind = 0.1
)

fviz_pca_biplot(res.pca,c(2,3),
                label = "var", # hide individual labels
                repel = TRUE,
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                alpha.ind = 0.1
)


